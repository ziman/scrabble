module Scrabble (game, mkInitialState) where

import Prelude hiding (log, Word, length)
import System.Random

import Safe (minimumMay)
import Data.Char
import Data.Functor
import Data.Function
import Data.Foldable hiding (length)
import Data.Text (Text)
import Data.Set (Set)
import Data.Map.Strict (Map)
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import qualified Data.Text as Text
import qualified VectorShuffling.Immutable as Vec
import qualified Data.Yaml as Yaml
import qualified Data.List as List

import Control.Monad (when)

import Game.WSGame.Game
import Game.WSGame.Engine (Connection)
import qualified Game.WSGame.Engine as Engine

import qualified Api

data Player = Player
  { name :: Text
  , letters :: [Api.Letter]
  , score :: Int
  , vote :: Bool
  , turns :: Int
  }

instance Show Player where
  show Player{name,score,letters}
    = show (name,score,letters)

type Board = Map (Int, Int) Api.Cell

newtype PlayerId = PlayerId {unPlayerId :: Int}
  deriving newtype (Eq, Ord, Show)

data State = State
  { players :: Map PlayerId Player
  , nextPlayerId :: PlayerId
  , connections :: Bimap Connection PlayerId
  , boardSize :: (Int, Int)
  , board :: Board
  , bag :: [Api.Letter]
  , uncommitted :: Map (Int, Int) PlayerId
  }
  deriving Show

data Effect
  = Send Connection Api.Message_S2C
  | Close Connection
  | Log String

data Self = Self
  { connection :: Connection
  , pid :: PlayerId
  , player :: Player
  }
  deriving Show

type Scrabble a = GameM State Effect () Connection a

log :: String -> Scrabble ()
log msg = perform $ Log msg

send :: Connection -> Api.Message_S2C -> Scrabble ()
send conn msg = perform $ Send conn msg

close :: Connection -> Scrabble ()
close conn = perform $ Close conn

getSelf :: Scrabble Self
getSelf = do
  State{players, connections} <- getState
  connection <- getConnection
  case Bimap.lookup connection connections of
    Nothing -> throwHard $ "connection not in game state: " ++ show connection
    Just pid -> case Map.lookup pid players of
      Nothing -> throwHard $ "no player associated with " ++ show connection
      Just player -> pure $ Self{connection, pid, player}

onDeadPlayer :: Scrabble ()
onDeadPlayer = do
  connection <- getConnection

  modifyState $ \st@State{connections} -> st
    { connections = Bimap.delete connection connections
    }

  broadcastStateUpdate

sendStateUpdate :: Connection -> Player -> State -> Scrabble ()
sendStateUpdate conn Player{..} st@State{boardSize=(rows,cols), ..} = do
  let mbNextTurn = minimumMay
        [ (turns, pid)
        | (pid, Player{turns}) <- Map.toList players
        , pid `Bimap.memberR` connections  -- and is alive
        ]
  send conn $ Api.Update $ Api.State
    { players =
      [ Api.Player
        { name    = name
        , score   = score
        , letters = List.length letters
        , vote    = vote
        , isAlive = pid `Bimap.memberR` connections
        , turns
        , isTheirTurn = Just (turns, pid) == mbNextTurn
        }
      | (pid, Player{..}) <- Map.toList players
      ]
    , board = Api.MkBoard  -- we could precompute this
      { rows = rows
      , cols = cols
      , cells =
        [ [ board Map.! (i, j)
          | j <- [0..cols-1]
          ]
        | i <- [0..rows-1]
        ]
      }
    , uncommitted = Map.keys uncommitted
    , uncommittedWords = getUncommittedWords st
    , bonus = getBonus uncommitted
    , lettersLeft = List.length bag

    -- player props
    , vote
    , letters
    , name
    }

broadcastStateUpdate :: Scrabble ()
broadcastStateUpdate = do
  st@State{players, connections} <- getState
  for_ (Bimap.toList connections) $ \(connection, pid) ->
    sendStateUpdate connection (players Map.! pid) st

resetVotes :: Scrabble ()
resetVotes = modifyState $ \st -> st
  { players = players st
    & Map.map (\p -> p{vote = False})
  }

extract :: Int -> [a] -> Maybe (a, [a])
extract _ [] = Nothing
extract 0 (x : xs) = Just (x, xs)
extract i (x : xs) =
  extract (i-1) xs <&>
    \(y, ys) -> (y, x:ys)

move :: Int -> Int -> [a] -> [a]
move i j xs
  | i < j
  , (xsA, xsBC) <- splitAt i xs
  , (b:xsB, xsC) <- splitAt (j-i+1) xsBC
  = xsA ++ xsB ++ b : xsC

  | i > j
  , (xsA, xsBC) <- splitAt j xs
  , (xsB, c:xsC) <- splitAt (i-j) xsBC
  = xsA ++ c : xsB ++ xsC

  | otherwise = xs

getBonus :: Map (Int, Int) PlayerId -> Int
getBonus uncommitted
  | Map.size uncommitted >= 7 = 50
  | otherwise = 0

checkVotes :: Scrabble ()
checkVotes = do
  st@State{players,uncommitted,bag} <- getState

  case Set.toList $ Set.fromList (Map.elems uncommitted) of
    -- exactly one player owns all letters
    [scorerId] -> do
      let agreed = all (\Player{vote} -> vote == True) (Map.elems players)
      when agreed $ do
        let wordScore = sum [value | Api.UncommittedWord{value} <- getUncommittedWords st]
        let (newLetters, bag') = splitAt (Map.size uncommitted) bag
        setState st
          { uncommitted = Map.empty
          , players = players
            & Map.adjust
                (\p@Player{score,letters,turns} -> p
                  { score = score + wordScore + getBonus uncommitted
                  , letters = letters ++ newLetters
                  , turns = turns + 1
                  }
                )
                scorerId
          , bag = bag'
          }
        resetVotes

    [] -> do
      -- undo voting
      resetVotes
      broadcastStateUpdate
      throwSoft "nothing to score"

    owners -> do
      -- undo voting
      resetVotes
      broadcastStateUpdate
      throwSoft $ "multiple letter owners: "
        ++ List.intercalate ", "
          [ Text.unpack name
          | conn <- owners
          , Just Player{name} <- [Map.lookup conn players]
          ]

data Word = Word
  { range :: ((Int,Int),(Int,Int))
  -- Range includes its endpoints.
  -- Used to deduplicate words and for sort order.
  , word :: Text
  , letterScore :: Int
  , wordMultiplier :: Int
  , length :: Int
  }
  deriving (Eq, Ord, Show)

instance Semigroup Word where
  Word ((minI,minJ),(maxI,maxJ)) w ls wm l
   <> Word ((minI',minJ'),(maxI',maxJ')) w' ls' wm' l'
    = Word
       (( minI `min` minI', minJ `min` minJ')
       ,( maxI `max` maxI', maxJ `max` maxJ'))
       (w <> w') (ls + ls') (wm * wm') (l + l')

instance Monoid Word where
  mempty = Word ((maxBound,maxBound),(minBound,minBound)) "" 0 1 0

getWords :: Int -> Int -> Board -> Map (Int, Int) PlayerId -> Set Word
getWords i j board uncommitted = Set.fromList $
  filter (\Word{length} -> length > 1)
    [ go (-1, 0) (i,j) <> go (1, 0) (i+1,j)
    , go (0, -1) (i,j) <> go (0, 1) (i,j+1)
    ]
  where
    go :: (Int, Int) -> (Int, Int) -> Word
    go _ (-1, _) = mempty
    go _ (_, -1) = mempty
    go _ (15, _) = mempty
    go _ (_, 15) = mempty
    go (di, dj) (i, j)
      | Just Api.Cell{letter = Just Api.Letter{letter,value}, boost}
          <- Map.lookup (i,j) board
      , rest <- go (di, dj) (i+di, j+dj)
      , (<+>) <- if di+dj < 0 then (<>) else flip (<>)
      , rng <- ((i,j),(i,j))
      , boostU <- if (i,j) `Map.member` uncommitted
          then boost
          else Nothing
      = case boostU of
          Just Api.DoubleLetter -> rest <+> Word rng letter (2*value) 1 1
          Just Api.TripleLetter -> rest <+> Word rng letter (3*value) 1 1
          Just Api.DoubleWord -> rest <+> Word rng letter value 2 1
          Just Api.TripleWord -> rest <+> Word rng letter value 3 1
          Nothing -> rest <+> Word rng letter value 1 1

      | otherwise = mempty

getUncommittedWords :: State -> [Api.UncommittedWord]
getUncommittedWords State{board,uncommitted} =
  [ Api.UncommittedWord
    { word
    , value = letterScore * wordMultiplier
    }
  | Word{word,letterScore,wordMultiplier}
      <- Set.toAscList $ Set.unions
        [ getWords i j board uncommitted
        | (i,j) <- Map.keys uncommitted
        ]
  ]

handle :: Api.Message_C2S -> Scrabble ()
handle Api.Join{playerName} = do
  st@State{players, connections, nextPlayerId} <- getState
  thisConnection <- getConnection

  -- check if this player already exists
  case [(pid, p) | (pid, p@Player{name}) <- Map.toList players, name == playerName] of
    -- existing player
    (pid, Player{name}):_ ->
      case [c | (c, pid') <- Bimap.toList connections, pid' == pid] of
        -- currently connected
        oldConnection:_ -> do
          log $ show thisConnection ++ " replaces live player "
            ++ show (oldConnection, name)

          -- replace the player
          setState st
            { connections = connections
              & Bimap.delete oldConnection
              & Bimap.insert thisConnection pid
            }

          -- close the old connection
          close oldConnection

        -- dead player
        [] -> do
          log $ show thisConnection ++ " resurrects dead player "
            ++ show name

          -- resurrect the player
          setState st
            { connections = connections
              & Bimap.insert thisConnection pid
            }

    -- brand new player
    [] -> do
      -- create a new player
      log $ show thisConnection ++ " is a new player"

      let (letters, rest) = splitAt 8 (bag st)
      setState st
        { players = players
          & Map.insert nextPlayerId Player
            { name = playerName
            , letters = letters
            , score = 0
            , vote = False
            , turns = foldl' max 0 [turns | Player{turns} <- Map.elems players]
            }
        , connections = connections
          & Bimap.insert thisConnection nextPlayerId
        , nextPlayerId = PlayerId (unPlayerId nextPlayerId + 1)
        , bag = rest
        }

  broadcastStateUpdate

handle Api.Drop{src, dst} = do
  st@State{uncommitted} <- getState
  Self{pid, player = player@Player{letters}} <- getSelf

  case (src, dst) of
    (Api.Letters k, Api.Board dstI dstJ)
      | Just cellDst@Api.Cell{letter = Nothing}
          <- Map.lookup (dstI, dstJ) (board st)
      , Just (letter, rest) <- extract k letters
      -> do
        setState st
          { board = board st
            & Map.insert (dstI, dstJ) (cellDst{Api.letter = Just letter} :: Api.Cell)
          , players = players st
            & Map.insert pid player{letters = rest}
          , uncommitted = uncommitted
            & Map.insert (dstI, dstJ) pid
          }

        resetVotes
        broadcastStateUpdate

    (Api.Board srcI srcJ, Api.Board dstI dstJ)
      | Just cellDst@Api.Cell{letter = Nothing}
          <- Map.lookup (dstI, dstJ) (board st)
      , Just cellSrc@Api.Cell{letter = Just letter}
          <- Map.lookup (srcI, srcJ) (board st)
      , Just ownerId <- Map.lookup (srcI, srcJ) uncommitted
      , ownerId == pid
      -> do
        setState st
          { board = board st
            & Map.insert (dstI, dstJ) (cellDst{Api.letter = Just letter} :: Api.Cell)
            & Map.insert (srcI, srcJ) (cellSrc{Api.letter = Nothing} :: Api.Cell)
          , uncommitted = uncommitted
            & Map.delete (srcI, srcJ)
            & Map.insert (dstI, dstJ) pid
          }

        resetVotes
        broadcastStateUpdate

    (Api.Letters srcIdx, Api.Letters dstIdx)
      | moved <- move srcIdx dstIdx letters
      -> do
        setState st
          { players = players st
            & Map.insert pid player{letters = moved}
          }
        broadcastStateUpdate

    (Api.Board srcI srcJ, Api.Letters dstIdx)
      | Just cellSrc@Api.Cell{letter = Just letter}
          <- Map.lookup (srcI, srcJ) (board st)
      , Just ownerId <- Map.lookup (srcI, srcJ) uncommitted
      , ownerId == pid
      -> do
        let (ls, rs) = splitAt dstIdx letters
        setState st
          { players = players st
            & Map.insert pid player{letters = ls ++ letter : rs}
          , board = board st
            & Map.insert (srcI, srcJ) (cellSrc{Api.letter = Nothing} :: Api.Cell)
          , uncommitted = uncommitted
            & Map.delete (srcI, srcJ)
          }
        broadcastStateUpdate

    _ -> throwSoft "can't move letter"

handle Api.Vote{vote} = do
  Self{pid, player} <- getSelf

  modifyState $ \st -> st
    { players = players st
      & Map.insert pid player{vote}
    }
  checkVotes
  broadcastStateUpdate

-- repeats are fine
symmetry :: [(Int, Int)] -> [(Int, Int)]
symmetry ijs = concat
  [ [ (i, j)
    , (14-i, j)
    , (i, 14-j)
    , (14-i, 14-j)
    , (j, i)
    , (j, 14-i)
    , (14-j, i)
    , (14-j, 14-i)
    ]
  | (i, j) <- ijs
  ]

boosts :: [(Api.Boost, [(Int, Int)])]
boosts =
  [ (Api.DoubleLetter, symmetry [(0, 3), (2, 6), (3, 7)])
  , (Api.TripleLetter, symmetry [(1, 5), (5, 5)])
  , (Api.DoubleWord, symmetry [(i, i) | i <- [1..4]])
  , (Api.TripleWord, symmetry [(0, 0), (0, 7)])
  ]

game :: Engine.Game State Effect () Api.Message_C2S Api.Message_S2C
game = Engine.Game
  { onMessage = handle
  , onDeadPlayer = Scrabble.onDeadPlayer
  , runEffect = Scrabble.runEffect
  }

mkInitialState :: FilePath -> IO State
mkInitialState fnLanguage = do
  lang <- Yaml.decodeFileThrow fnLanguage
  g <- newStdGen
  pure $ State
    { players = Map.empty
    , connections = Bimap.empty
    , nextPlayerId = PlayerId 1
    , boardSize = (15, 15)
    , board =
      Map.fromList
        [(ij, Api.Cell (Just boost) Nothing) | (boost, ijs) <- boosts, ij <- ijs]
      `Map.union`
        Map.fromList
          [((i,j), Api.Cell Nothing Nothing) | i <- [0..14], j <- [0..14]]
    , bag = shuffle g $ languageLetters lang
    , uncommitted = mempty
    }
  where
    shuffle :: StdGen -> [a] -> [a]
    shuffle g = Vec.toList . fst . flip Vec.shuffle g . Vec.fromList

    languageLetters :: Map Int (Map Text Int) -> [Api.Letter]
    languageLetters lang = concat
      [ replicate count Api.Letter
        { letter = Text.map toUpper letter
        , value
        }
      | (value, letterCnts) <- Map.toList lang
      , (letter, count) <- Map.toList letterCnts
      ]

runEffect :: Effect -> IO ()
runEffect = \case
  Log msg -> putStrLn msg
  Close conn -> Engine.close @Api.Message_S2C conn
  Send conn msg -> Engine.send conn msg
