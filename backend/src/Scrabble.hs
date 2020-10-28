module Scrabble (game, mkInitialState) where

import Prelude hiding (log)
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

import Control.Monad.Trans.RWS.CPS hiding (state)

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

data State = State
  { players :: Map Cookie Player
  , boardSize :: (Int, Int)
  , board :: Map (Int, Int) Api.Cell
  , bag :: [Api.Letter]
  , uncommitted :: Set (Int, Int)
  }
  deriving Show

type Scrabble a = GameM State Api.Message_S2C a

the :: (a -> b) -> a -> b
the proj = proj

getPlayer :: Scrabble Player
getPlayer = do
  st <- getState
  cookie <- the @(Env State) cookie <$> ask
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
sendStateUpdate conn Player{..} st = do
  let (rows, cols) = boardSize st
  send conn $ Api.Update $ Api.State
    { players =
      [ Api.Player
        { name    = name
        , score   = score
        , letters = length letters
        , isAlive = isJust connection
        , vote    = vote
        }
      | Player{..} <- Map.elems (players st)
      ]
    , board = Api.MkBoard  -- we could precompute this
      { rows = rows
      , cols = cols
      , cells =
        [ [ board st Map.! (i, j)
          | j <- [0..cols-1]
          ]
        | i <- [0..rows-1]
        ]
      }
    , uncommitted = Set.toList (uncommitted st)

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
  st <- getState
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
          , uncommitted = uncommitted st
            & Set.insert (dstI, dstJ)
          }

        resetVotes
        broadcastStateUpdate

    (Api.Board srcI srcJ, Api.Board dstI dstJ)
      | Just cellDst@Api.Cell{letter = Nothing}
          <- Map.lookup (dstI, dstJ) (board st)
      , Just cellSrc@Api.Cell{letter = Just letter}
          <- Map.lookup (srcI, srcJ) (board st)
      -> do
        setState st
          { board = board st
            & Map.insert (dstI, dstJ) (cellDst{Api.letter = Just letter} :: Api.Cell)
            & Map.insert (srcI, srcJ) (cellSrc{Api.letter = Nothing} :: Api.Cell)
          , uncommitted = uncommitted st
            & Set.delete (srcI, srcJ)
            & Set.insert (dstI, dstJ)
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
      -> do
        let (ls, rs) = splitAt dstIdx (letters player)
        setState st
          { players = players st
            & Map.insert cookie player{letters = ls ++ letter : rs}
          , board = board st
            & Map.insert (srcI, srcJ) (cellSrc{Api.letter = Nothing} :: Api.Cell)
          , uncommitted = uncommitted st
            & Set.delete (srcI, srcJ)
          }
        broadcastStateUpdate

    _ -> throwSoft "can't drop there"


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
