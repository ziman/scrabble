{-# LANGUAGE TemplateHaskell #-}
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
import Control.Monad.State.Class

import Game.WSGame.Engine (Connection)
import Game.WSGame.Game as Game
import qualified Game.WSGame.Engine as Engine

import Lens.Micro.Platform

import qualified Api

data Player = Player
  { _name :: Text
  , _letters :: [Api.Letter]
  , _score :: Int
  , _vote :: Bool
  , _turns :: Int
  }

makeLenses ''Player

instance Show Player where
  show Player{_name, _score, _letters}
    = show (_name, _score, _letters)

-- always 15x15
type Board = Map (Int, Int) Api.Cell

newtype PlayerId = PlayerId {unPlayerId :: Int}
  deriving newtype (Eq, Ord, Show)

data State = State
  { _players :: Map PlayerId Player
  , _nextPlayerId :: PlayerId
  , _connections :: Bimap Connection PlayerId
  , _board :: Board
  , _bag :: [Api.Letter]
  , _uncommitted :: Map (Int, Int) PlayerId
  , _stdGen :: StdGen
  }
  deriving Show

makeLenses ''State

data Effect
  = Send Connection Api.Message_S2C
  | Close Connection
  | Log String

type Scrabble a = Game.GameM State Effect () Connection a

log :: String -> Scrabble ()
log msg = perform $ Log msg

send :: Connection -> Api.Message_S2C -> Scrabble ()
send conn msg = perform $ Send conn msg

close :: Connection -> Scrabble ()
close conn = perform $ Close conn

data Self = Self PlayerId (Traversal' State Player)

getSelf :: Scrabble Self
getSelf = do
  state <- getState
  connection <- getConnection
  case Bimap.lookup connection (state ^. connections) of
    Nothing -> throwHard $ "connection not in game state: " ++ show connection
    Just pid -> do
      -- check the PID because `ix` won't do that
      when (not $ has (players . ix pid) state) $
        throwHard $ "no player for PID " ++ show pid

      return $ Self pid (players . ix pid)

onDeadPlayer :: Scrabble ()
onDeadPlayer = do
  connection <- getConnection
  connections %= Bimap.delete connection
  broadcastStateUpdate

sendStateUpdate :: Connection -> Player -> State -> Scrabble ()
sendStateUpdate conn player st = do
  let mbNextTurn = minimumMay
        [ (p ^. turns, pid)
        | (pid, p) <- Map.toList (st ^. players)
        , pid `Bimap.memberR` (st ^. connections)  -- and is alive
        ]
  send conn $ Api.Update $ Api.State
    { players =
      [ Api.Player
        { name    = p ^. name
        , score   = p ^. score
        , letters = List.length (p ^. letters)
        , vote    = p ^. vote
        , isAlive = pid `Bimap.memberR` (st ^. connections)
        , turns   = p ^. turns
        , isTheirTurn = Just (p ^. turns, pid) == mbNextTurn
        }
      | (pid, p) <- Map.toList (st ^. players)
      ]
    , board = Api.MkBoard  -- we could precompute this
      { rows = 15
      , cols = 15
      , cells =
        [ [ (st ^. board) Map.! (i, j)
          | j <- [0..14]
          ]
        | i <- [0..14]
        ]
      }
    , uncommitted = Map.keys (st ^. uncommitted)
    , uncommittedWords = getUncommittedWords st
    , bonus = getBonus (st ^. uncommitted)
    , lettersLeft = List.length (st ^. bag)

    -- player props
    , vote    = player ^. vote
    , letters = player ^. letters
    , name    = player ^. name
    }

broadcastStateUpdate :: Scrabble ()
broadcastStateUpdate = do
  st <- getState
  for_ (Bimap.toList $ st ^. connections) $ \(connection, pid) ->
    sendStateUpdate connection ((st ^. players) Map.! pid) st

resetVotes :: Scrabble ()
resetVotes =
  players.each.vote .= False

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
  st <- getState

  case Set.toList $ Set.fromList (Map.elems (st ^. uncommitted)) of
    -- exactly one player owns all letters
    [scorerId] -> do
      let agreed = and (st ^.. players . each . vote)
      when agreed $ do
        let wordScore = sum [value | Api.UncommittedWord{value} <- getUncommittedWords st]
        let (newLetters, bag') = splitAt (Map.size (st ^. uncommitted)) (st ^. bag)

        modify $ \st -> st
          & uncommitted .~ Map.empty
          & bag .~ bag'
          & players . ix scorerId %~
             \p -> p
              & (score   %~ (\s -> s + wordScore + getBonus (st ^. uncommitted)))
              & (letters %~ (++ newLetters))
              & (turns   %~ (+1))

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
          [ Text.unpack (p ^. name)
          | conn <- owners
          , Just p <- [st ^. players . at conn]
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
getUncommittedWords st =
  [ Api.UncommittedWord
    { word
    , value = letterScore * wordMultiplier
    }
  | Word{word,letterScore,wordMultiplier}
      <- Set.toAscList $ Set.unions
        [ getWords i j (st ^. board) (st ^. uncommitted)
        | (i,j) <- Map.keys (st ^. uncommitted)
        ]
  ]

handle :: Api.Message_C2S -> Scrabble ()
handle Api.Join{playerName} = do
  st <- getState
  thisConnection <- getConnection

  -- check if this player already exists
  case [(pid, p) | (pid, p) <- Map.toList (st ^. players), (p ^. name) == playerName] of
    -- existing player
    (pid, player):_ ->
      case [c | (c, pid') <- Bimap.toList connections, pid' == pid] of
        -- currently connected
        oldConnection:_ -> do
          log $ show thisConnection ++ " replaces live player "
            ++ show (oldConnection, player ^. name)

          -- replace the player
          connections %=
              (at oldConnection  .~ Nothing)
            . (at thisConnection .~ Just pid)

          -- close the old connection
          close oldConnection

        -- dead player
        [] -> do
          log $ show thisConnection ++ " resurrects dead player "
            ++ show (player ^. name)

          -- resurrect the player
          connections . at thisConnection .= Just pid

    -- brand new player
    [] -> do
      -- create a new player
      log $ show thisConnection ++ " is a new player"

      let (letters, rest) = splitAt 8 (bag st)
      modify $ \st -> st
        & (players . at (st ^. nextPlayerId) .~ Just Player
            { _name = playerName
            , _letters = letters
            , _score = 0
            , _vote = False
            , _turns = foldl' max 0 (st ^.. players . each . turns)
            }
          )
        & (connections . at thisConnection .~ Just (st ^. nextPlayerId))
        & (nextPlayerId %~ (PlayerId . (+1) . unPlayerId))
        & (bag .~ rest)

  broadcastStateUpdate

handle Api.Drop{src, dst} = do
  st <- getState
  Self pid self <- getSelf

  case (src, dst) of
    (Api.Letters k, Api.Board dstI dstJ)
      | Just dstCell@Api.Cell{letter = Nothing}
          <- st ^. board . at (dstI, dstJ)
      , Just (srcLetter, rest)
          <- extract k (st ^. self . letters)
      -> do
        modify $
            (board . ix (dstI, dstJ) .~ (dstCell{Api.letter = srcLetter} :: Api.Cell))
          . (self . letters .~ rest)
          . (uncommitted . at (dstI, dstJ) .~ Just pid)

        resetVotes
        broadcastStateUpdate

    (Api.Board srcI srcJ, Api.Board dstI dstJ)
      | Just dstCell@Api.Cell{letter = Nothing}
          <- st ^. board . at (dstI, dstJ)
      , Just srcCell@Api.Cell{letter = Just srcLetter}
          <- st ^. board . at (srcI, srcJ)
      , Just ownerId
          <- st ^. uncommitted . at (srcI, srcJ)
      , ownerId == pid
      -> do
        modify $
            (board
              & (ix (dstI, dstJ) .~ (dstCell{Api.letter = Just srcLetter} :: Api.Cell))
              & (ix (srcI, srcJ) .~ (srcCell{Api.letter = Nothing} :: Api.Cell)))
          . (uncommitted
              & (at (srcI, srcJ) .~ Nothing)
              & (at (dstI, dstJ) .~ Just pid))

        resetVotes
        broadcastStateUpdate

    (Api.Letters srcIdx, Api.Letters dstIdx)
      | moved <- move srcIdx dstIdx letters
      -> do
        self . letters .= moved
        broadcastStateUpdate

    (Api.Board srcI srcJ, Api.Letters dstIdx)
      | Just srcCell@Api.Cell{letter = Just srcLetter}
          <- st ^. board . at (srcI, srcJ)
      , Just ownerId
          <- st ^. uncommitted . at (srcI, srcJ)
      , ownerId == pid
      -> do
        let (ls, rs) = splitAt dstIdx letters

        modify $
            (self . letters .~ (ls ++ srcLetter : rs))
          . (board . ix (srcI, srcJ) .~ (srcCell{Api.letter = Nothing} :: Api.Cell))
          . (uncommitted . at (srcI, srcJ) .~ Nothing)

        broadcastStateUpdate

    _ -> throwSoft "can't move letter"

handle Api.Vote{vote} = do
  Self _pid self <- getSelf
  self . vote .= vote
  checkVotes
  broadcastStateUpdate

handle Api.Recycle = do
  Self _pid self <- getSelf

  when (not $ Map.null uncommitted) $
    throwSoft "can't recycle with uncommitted letters on board"

  bigBag <- (letters ++) <$> use bag
  let (stdGen', bigBag') = shuffle stdGen bigBag
  let (letters', bag') = splitAt 8 bigBag'

  modify $
      (self . turns %~ (+1))
    . (self . letters .~ letters)
    . (stdGen .~ stdGen')
    . (bag .~ bag')

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

shuffle :: StdGen -> [a] -> (StdGen, [a])
shuffle g xs =
  let (xs', g') = Vec.shuffle (Vec.fromList xs) g
    in (g', Vec.toList xs')

mkInitialState :: FilePath -> IO State
mkInitialState fnLanguage = do
  lang <- Yaml.decodeFileThrow fnLanguage
  g <- newStdGen
  let (stdGen, bag) = shuffle g $ languageLetters lang
  pure $ State
    { _players = Map.empty
    , _connections = Bimap.empty
    , _nextPlayerId = PlayerId 1
    , _board =
      Map.fromList
        [(ij, Api.Cell (Just boost) Nothing) | (boost, ijs) <- boosts, ij <- ijs]
      `Map.union`
        Map.fromList
          [((i,j), Api.Cell Nothing Nothing) | i <- [0..14], j <- [0..14]]
    , _uncommitted = mempty
    , _bag = bag
    , _stdGen = stdGen
    }
  where
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
