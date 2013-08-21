module Network.Server.TicTacToe.Game where

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
