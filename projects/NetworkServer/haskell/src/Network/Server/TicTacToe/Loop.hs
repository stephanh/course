module Network.Server.TicTacToe.Loop where

import Network.Server.Common.Env
import Control.Monad.Trans(MonadIO(..), MonadTrans(..))
import Control.Monad(liftM)

data Loop v s f a =
  Loop (Env v -> s -> f (a, s))

instance Functor f => Functor (Loop v s f) where
  fmap f (Loop k) =
    Loop (\env -> fmap (\(a, t) -> (f a, t)) . k env)

instance Monad f => Monad (Loop v s f) where
  return a =
    Loop $ \_ s -> return (a, s)
  Loop k >>= f =
    Loop (\env s -> k env s >>= \(a, t) ->
      let Loop l = f a
      in l env t)

instance MonadTrans (Loop v s) where
  lift x =
    Loop (\_ s -> liftM (\a -> (a, s)) x)

instance MonadIO f => MonadIO (Loop v s f) where
  liftIO =
    lift . liftIO
