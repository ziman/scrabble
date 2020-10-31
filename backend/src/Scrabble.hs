module Scrabble (game, mkInitialState) where

import Prelude hiding (log, Word, length)
import System.Random

import Data.Char
import Data.Functor
import Data.Function
import Data.Foldable hiding (length)
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
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
  }

instance Show Player where
  show Player{name,score,letters}
    = show (name,score,letters)

type Board = Map (Int, Int) Api.Cell

data State = State
  { players :: Map Connection Player
  , deadPlayers :: [Player]
  , boardSize :: (Int, Int)
  , board :: Board
  , bag :: [Api.Letter]
  , uncommitted :: Map (Int, Int) Connection
  }
  deriving Show

data Effect
  = Send Connection Api.Message_S2C
  | Close Connection
  | Log String

type Scrabble a = GameM State Effect Connection a

log :: String -> Scrabble ()
log msg = perform $ Log msg

send :: Connection -> Api.Message_S2C -> Scrabble ()
send conn msg = perform $ Send conn msg

close :: Connection -> Scrabble ()
close conn = perform $ Close conn

getPlayer :: Scrabble Player
getPlayer = do
  st <- getState
  connection <- getConnection
  case Map.lookup connection (players st) of
    Nothing -> throwHard $ "no player associated with " ++ show connection
    Just player -> pure player

onDeadPlayer :: Scrabble ()
onDeadPlayer = do
  player <- getPlayer
  connection <- getConnection

  modifyState $ \st@State{players,deadPlayers} -> st
    { players = Map.delete connection players
    , deadPlayers = player : deadPlayers
    }

  broadcastStateUpdate

sendStateUpdate :: Connection -> Player -> State -> Scrabble ()
sendStateUpdate conn Player{..} st@State{boardSize=(rows,cols), ..} = do
  send conn $ Api.Update $ Api.State
    { players =
      [ Api.Player
        { name    = name
        , score   = score
        , letters = List.length letters
        , vote    = vote
        , isAlive
        }
      | (isAlive, Player{..})
          <- zip (repeat True) (Map.elems players)
            ++ zip (repeat False) deadPlayers
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

    -- player props
    , vote
    , letters
    , name
    }

broadcastStateUpdate :: Scrabble ()
broadcastStateUpdate = do
  st <- getState
  for_ (Map.toList $ players st) $ \(conn, player) ->
    sendStateUpdate conn player st

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

checkVotes :: Scrabble ()
checkVotes = do
  st@State{players,uncommitted} <- getState

  case Set.toList $ Set.fromList (Map.elems uncommitted) of
    -- exactly one player owns all letters
    [connScorer] -> do
      let cntAlive = Map.size players
          cntVoting = List.length [() | Player{vote = True} <- Map.elems players]
      when (cntVoting >= (cntAlive+1) `div` 2) $ do
        let wordScore = sum [value | Api.UncommittedWord{value} <- getUncommittedWords st]
        setState st
          { uncommitted = Map.empty
          , players = players
            & Map.adjust
                (\p@Player{score} -> p{score = score + wordScore})
                connScorer
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

getWords :: Int -> Int -> Board -> Map (Int, Int) Connection -> Set Word
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
        + if length >= 7
            then 50
            else 0
    }
  | Word{word,letterScore,wordMultiplier, length}
      <- Set.toAscList $ Set.unions
        [ getWords i j board uncommitted
        | (i,j) <- Map.keys uncommitted
        ]
  ]

handle :: Api.Message_C2S -> Scrabble ()
handle Api.Join{playerName} = do
  st@State{players, deadPlayers} <- getState
  thisConnection <- getConnection

  -- check if this player already exists
  case [(c, p) | (c, p@Player{name}) <- Map.toList players, name == playerName] of
    -- it is a live player, take over session
    (oldConnection, player):_ -> do
      log $ show thisConnection ++ " replaces live player " ++ show (oldConnection, name player)

      -- replace the player
      setState st
        { players = players
          & Map.delete oldConnection
          & Map.insert thisConnection player
        }

      -- close the old connection
      close oldConnection

    -- not a live player, maybe they're dead?
    _ -> case List.partition (\Player{name} -> name == playerName) deadPlayers of
      -- resurrected player
      ([player@Player{name}], rest) -> do
        log $ show thisConnection ++ " resurrects dead player " ++ show name

        -- resurrect the player
        setState st
          { players = players
            & Map.insert thisConnection player
          , deadPlayers = rest
          }

      -- brand new player
      _ -> do
        -- create a new player
        log $ show thisConnection ++ " is a new player"

        let (letters, rest) = splitAt 8 (bag st)
        setState st
          { players = players
            & Map.insert thisConnection Player
              { name = playerName
              , letters = letters
              , score = 0
              , vote = False
              }
            , bag = rest
            }

  broadcastStateUpdate

handle Api.Drop{src, dst} = do
  st@State{uncommitted} <- getState
  connection <- getConnection
  player <- getPlayer

  case (src, dst) of
    (Api.Letters k, Api.Board dstI dstJ)
      | Just cellDst@Api.Cell{letter = Nothing}
          <- Map.lookup (dstI, dstJ) (board st)
      , Just (letter, rest) <- extract k (letters player)
      -> do
        setState st
          { board = board st
            & Map.insert (dstI, dstJ) (cellDst{Api.letter = Just letter} :: Api.Cell)
          , players = players st
            & Map.insert connection player{letters = rest}
          , uncommitted = uncommitted
            & Map.insert (dstI, dstJ) connection
          }

        resetVotes
        broadcastStateUpdate

    (Api.Board srcI srcJ, Api.Board dstI dstJ)
      | Just cellDst@Api.Cell{letter = Nothing}
          <- Map.lookup (dstI, dstJ) (board st)
      , Just cellSrc@Api.Cell{letter = Just letter}
          <- Map.lookup (srcI, srcJ) (board st)
      , Just ownerConnection <- Map.lookup (srcI, srcJ) uncommitted
      , ownerConnection == connection
      -> do
        setState st
          { board = board st
            & Map.insert (dstI, dstJ) (cellDst{Api.letter = Just letter} :: Api.Cell)
            & Map.insert (srcI, srcJ) (cellSrc{Api.letter = Nothing} :: Api.Cell)
          , uncommitted = uncommitted
            & Map.delete (srcI, srcJ)
            & Map.insert (dstI, dstJ) connection
          }

        resetVotes
        broadcastStateUpdate

    (Api.Letters srcIdx, Api.Letters dstIdx)
      | moved <- move srcIdx dstIdx (letters player)
      -> do
        setState st
          { players = players st
            & Map.insert connection player{letters = moved}
          }
        broadcastStateUpdate

    (Api.Board srcI srcJ, Api.Letters dstIdx)
      | Just cellSrc@Api.Cell{letter = Just letter}
          <- Map.lookup (srcI, srcJ) (board st)
      , Just ownerConnection <- Map.lookup (srcI, srcJ) uncommitted
      , ownerConnection == connection
      -> do
        let (ls, rs) = splitAt dstIdx (letters player)
        setState st
          { players = players st
            & Map.insert connection player{letters = ls ++ letter : rs}
          , board = board st
            & Map.insert (srcI, srcJ) (cellSrc{Api.letter = Nothing} :: Api.Cell)
          , uncommitted = uncommitted
            & Map.delete (srcI, srcJ)
          }
        broadcastStateUpdate

    _ -> throwSoft "can't move letter"


handle Api.GetLetter = do
  st@State{players} <- getState
  player@Player{letters} <- getPlayer
  connection <- getConnection

  case bag st of
    [] -> throwSoft "no more letters"
    l:ls -> do
      setState st
        { bag = ls
        , players =
            Map.insert
              connection
              player{ letters = letters ++ [l] }
              players
        }
      broadcastStateUpdate

handle Api.Vote{vote} = do
  player <- getPlayer
  connection <- getConnection

  modifyState $ \st -> st
    { players = players st
      & Map.insert connection player{vote}
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

game :: Engine.Game State Effect Api.Message_C2S Api.Message_S2C
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
    , deadPlayers = []
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
