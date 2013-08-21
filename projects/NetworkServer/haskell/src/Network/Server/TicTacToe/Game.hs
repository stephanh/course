module Network.Server.TicTacToe.Game where

{-
import Prelude hiding (elem, mapM_, concat, catch)

import Network.Server.Common.Lens
import Data.TicTacToe

import Data.IORef(IORef, atomicModifyIORef)
import Control.Monad(liftM)
import Control.Monad.Trans(MonadTrans(..), MonadIO(..))
import Control.Applicative(Applicative(..))
import Control.Exception(IOException)

import Network.Server.TicTacToe.Command
import Network.Server.TicTacToe.GameEnv(FinishedGames, GameEnv(..), finishedGamesL)

newtype Game f a =
  Game (GameEnv -> f (a, Unfinished, FinishedGames))
        {-
fGame ::
  Functor f =>
  (GameEnv -> f (a, Unfinished))
  -> Game f a
fGame f =
  Game (\env -> fmap (\(a, b) -> (a, b, finishedGamesL `getL` env)) . f $ env)

fGame' ::
  Monad f =>
  (GameEnv -> f (a, Unfinished))
  -> Game f a
fGame' f =
  Game (\env -> liftM (\(a, b) -> (a, b, finishedGamesL `getL` env)) . f $ env)

rGame ::
  Functor f =>
  (GameEnv -> f a)
  -> Game f a
rGame f =
  fGame (\env -> fmap (\a -> (a, boardL `getL` env)) (f env))

rGame' ::
  Monad f =>
  (GameEnv -> f a)
  -> Game f a
rGame' f =
  fGame' (\env -> liftM (\a -> (a, boardL `getL` env)) . f $ env)

idGame ::
  Applicative f =>
  Game f GameEnv
idGame =
  rGame pure
                      -}
instance Functor f => Functor (Game f) where
  fmap =
    error "todo"

instance Monad f => Monad (Game f) where
  return =
    error "todo"
  (>>=) =
    error "todo"

instance MonadTrans Game where
  lift =
    error "todo"

instance MonadIO f => MonadIO (Game f) where
  liftIO =
    error "todo"

xprint ::
  IOException
  -> Game IO ()
xprint =
  liftIO . print

atomicModifyIORef_ ::
  IORef a
  -> (a -> a)
  -> IO ()
atomicModifyIORef_ r f =
  atomicModifyIORef r (\a -> (f a, ()))

data AtomicMove =
  IsOccupied
  | OutOfDate
  | MoveMade Board
  | GameOver FinishedBoard
  deriving (Eq, Show)

processCommand ::
  Command
  -> Game IO ()
processCommand =
  error "todo"

server ::
  Game IO ()
  -> IO ()
server =
  error "todo"

game ::
  Game IO ()
game =
  error "todo"
  -}

import Data.TicTacToe
import Network.Server.Common.Env
import Network.Server.Common.Line
import Network.Server.TicTacToe.Loop
import Data.Maybe(fromMaybe)
import Data.Foldable(msum)
import Control.Applicative((<$), (<$>))
import Control.Monad.Trans(MonadIO(..))


type FinishedGames =
  [FinishedBoard]

type Game a =
  IORefLoop Integer (Unfinished, FinishedGames) a


                               {-

data ChatCommand =
  Chat String
  | Incr
  | Unknown String
  deriving (Eq, Show)

incr ::
  Chat Integer
incr =
  do e <- readEnvval
     liftIO $ atomicModifyIORef_ e (+1)

chat ::
  Chat x -- client accepted (post)
  -> (String -> Chat w) -- read line from client
  -> IO a
chat =
  iorefLoop 0

-- |
--
-- >>> chatCommand "CHAT hi"
-- Chat "hi"
--
-- >>> chatCommand "Chat bye"
-- Chat "bye"
--
-- >>> chatCommand "INCR"
-- Incr
--
-- >>> chatCommand "Nothing"
-- UNKNOWN "Nothing"
chatCommand ::
  String
  -> ChatCommand
chatCommand z =
  Unknown z `fromMaybe` msum [
                               Chat <$> trimPrefixThen "CHAT" z
                             , Incr <$ trimPrefixThen "INCR" z
                             ]

process ::
  ChatCommand
  -> Chat ()
process (Chat m) =
  allClientsButThis ! "CHAT " ++ m
process Incr =
  do n <- incr
     pPutStrLn ("INCR " ++ show n)
process (Unknown s) =
  pPutStrLn ("UNKNOWN " ++ s)
                                 -}
