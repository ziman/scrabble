module Scrabble (game, mkInitialState) where

import Prelude hiding (log, Word)
import System.Random

import Data.Char
import Data.Functor
import Data.Function
import Data.Foldable
import Data.Text (Text)
import Data.Maybe (isJust)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import qualified Data.Text as Text
import qualified VectorShuffling.Immutable as Vec
import qualified Data.Yaml as Yaml

import Control.Monad (when)
import qualified Network.WebSockets as WS

import Game
import Engine
import qualified Api

data Player = Player
  { name :: Text
  , connection :: Maybe WS.Connection
  , letters :: [Api.Letter]
  , score :: Int
  , cookie :: Cookie
  , vote :: Bool
  }

instance Show Player where
  show Player{name,cookie,score,letters}
    = show (name, cookie, score, letters)

type Board = Map (Int, Int) Api.Cell

data State = State
  { players :: Map Cookie Player
  , boardSize :: (Int, Int)
  , board :: Board
  , bag :: [Api.Letter]
  , uncommitted :: Map (Int, Int) Cookie
  }
  deriving Show

type Scrabble a = GameM State Api.Message_S2C a

getPlayer :: Scrabble Player
getPlayer = do
  st <- getState
  cookie <- getCookie
  case Map.lookup cookie (players st) of
    Nothing -> throwHard $ "could not resolve cookie " ++ show cookie
    Just player -> pure player

onDeadPlayer :: Scrabble ()
onDeadPlayer = do
  player@Player{cookie} <- getPlayer
  modifyState $ \st -> st
    { players =
        Map.insert
          cookie
          (player{connection = Nothing} :: Player)
          (players st)
    }
  broadcastStateUpdate

sendStateUpdate :: WS.Connection -> Player -> State -> Scrabble ()
sendStateUpdate conn Player{..} st@State{boardSize=(rows,cols), ..} = do
  send conn $ Api.Update $ Api.State
    { players =
      [ Api.Player
        { name    = name
        , score   = score
        , letters = length letters
        , isAlive = isJust connection
        , vote    = vote
        }
      | Player{..} <- Map.elems players
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
    , cookie
    }

broadcastStateUpdate :: Scrabble ()
broadcastStateUpdate = do
  st <- getState
  for_ (Map.elems $ players st) $ \player@Player{connection} ->
    case connection of
      Nothing -> pure ()  -- can't update
      Just conn -> sendStateUpdate conn player st

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
  State{players} <- getState

  let cntAlive = length [() | Player{connection = Just _} <- Map.elems players]
      cntVoting = length [() | Player{connection = Just _, vote = True} <- Map.elems players]

  when (cntVoting >= (cntAlive+1) `div` 2) $ do
    -- TODO: compute score
    modifyState $ \st -> st{ uncommitted = Map.empty }
    resetVotes

data Word = Word
  { range :: ((Int,Int),(Int,Int))
  -- Range includes its endpoints.
  -- Used to deduplicate words and for sort order.
  , word :: Text
  , letterScore :: Int
  , wordMultiplier :: Int
  }
  deriving (Eq, Ord, Show)

instance Semigroup Word where
  Word ((minI,minJ),(maxI,maxJ)) w ls wm
   <> Word ((minI',minJ'),(maxI',maxJ')) w' ls' wm'
    = Word
       (( minI `min` minI', minJ `min` minJ')
       ,( maxI `max` maxI', maxJ `max` maxJ'))
       (w <> w') (ls + ls') (wm * wm')

instance Monoid Word where
  mempty = Word ((maxBound,maxBound),(minBound,minBound)) "" 0 1

getWords :: Int -> Int -> Board -> Map (Int, Int) Cookie -> Set Word
getWords i j board uncommitted = Set.fromList $
  filter (\Word{word} -> Text.length word > 1)
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
          Just Api.DoubleLetter -> rest <+> Word rng letter (2*value) 1
          Just Api.TripleLetter -> rest <+> Word rng letter (3*value) 1
          Just Api.DoubleWord -> rest <+> Word rng letter value 2
          Just Api.TripleWord -> rest <+> Word rng letter value 3
          Nothing -> rest <+> Word rng letter value 1

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
  st <- getState
  thisCookie <- getCookie
  thisConnection <- getConnection

  -- check if this player already exists
  case [p | p <- Map.elems (players st), name p == playerName] of
    -- it is an existing player, take over session
    oldPlayer@Player{cookie=oldCookie, connection=oldConnection}:_ -> do
      log $ show thisCookie ++ " resurrects player " ++ show (oldCookie, name oldPlayer)

      -- replace the player
      setState st
        { players = players st
          & Map.insert thisCookie (oldPlayer
            { cookie = thisCookie
            , connection = Just thisConnection
            } :: Player)
          & Map.delete oldCookie
        }

      -- close the connection in case it's still alive
      case oldConnection of
        Just conn -> close conn
        Nothing -> log $ "  -> old player already dead"

    _ -> do
      -- create a new player
      let (letters, rest) = splitAt 8 (bag st)
      setState st
        { players = players st
          & Map.insert thisCookie Player
            { name = playerName
            , connection = Just thisConnection
            , letters = letters
            , score = 0
            , cookie = thisCookie
            , vote = False
            }
          , bag = rest
          }
      log $ show thisCookie ++ " is a new player"

  broadcastStateUpdate

handle Api.Drop{src, dst} = do
  st@State{uncommitted} <- getState
  player@Player{cookie} <- getPlayer

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
            & Map.insert cookie player{letters = rest}
          , uncommitted = uncommitted
            & Map.insert (dstI, dstJ) cookie
          }

        resetVotes
        broadcastStateUpdate

    (Api.Board srcI srcJ, Api.Board dstI dstJ)
      | Just cellDst@Api.Cell{letter = Nothing}
          <- Map.lookup (dstI, dstJ) (board st)
      , Just cellSrc@Api.Cell{letter = Just letter}
          <- Map.lookup (srcI, srcJ) (board st)
      , Just ownerCookie <- Map.lookup (srcI, srcJ) uncommitted
      , ownerCookie == cookie
      -> do
        setState st
          { board = board st
            & Map.insert (dstI, dstJ) (cellDst{Api.letter = Just letter} :: Api.Cell)
            & Map.insert (srcI, srcJ) (cellSrc{Api.letter = Nothing} :: Api.Cell)
          , uncommitted = uncommitted
            & Map.delete (srcI, srcJ)
            & Map.insert (dstI, dstJ) cookie
          }

        resetVotes
        broadcastStateUpdate

    (Api.Letters srcIdx, Api.Letters dstIdx)
      | moved <- move srcIdx dstIdx (letters player)
      -> do
        setState st
          { players = players st
            & Map.insert cookie player{letters = moved}
          }
        broadcastStateUpdate

    (Api.Board srcI srcJ, Api.Letters dstIdx)
      | Just cellSrc@Api.Cell{letter = Just letter}
          <- Map.lookup (srcI, srcJ) (board st)
      , Just ownerCookie <- Map.lookup (srcI, srcJ) uncommitted
      , ownerCookie == cookie
      -> do
        let (ls, rs) = splitAt dstIdx (letters player)
        setState st
          { players = players st
            & Map.insert cookie player{letters = ls ++ letter : rs}
          , board = board st
            & Map.insert (srcI, srcJ) (cellSrc{Api.letter = Nothing} :: Api.Cell)
          , uncommitted = uncommitted
            & Map.delete (srcI, srcJ)
          }
        broadcastStateUpdate

    _ -> throwSoft "can't move letter"


handle Api.GetLetter = do
  st <- getState
  player@Player{cookie} <- getPlayer

  case bag st of
    [] -> throwSoft "no more letters"
    l:ls -> do
      setState st
        { bag = ls
        , players =
            Map.insert
              cookie
              player{ letters = letters player ++ [l] }
              (players st)
        }
      broadcastStateUpdate

handle Api.Vote{vote} = do
  player@Player{cookie} <- getPlayer
  modifyState $ \st -> st
    { players = players st
      & Map.insert cookie player{vote}
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

game :: Game State Api.Message_C2S Api.Message_S2C
game = Game{onMessage = handle, onDeadPlayer = Scrabble.onDeadPlayer}

mkInitialState :: FilePath -> IO State
mkInitialState fnLanguage = do
  lang <- Yaml.decodeFileThrow fnLanguage
  g <- newStdGen
  pure $ State
    { players = Map.empty
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
