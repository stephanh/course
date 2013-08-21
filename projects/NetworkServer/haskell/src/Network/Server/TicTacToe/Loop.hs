module Network.Server.TicTacToe.Loop where

import Prelude hiding (mapM_)
import Network.Server.Common.Env
import System.IO(BufferMode(..))
import Network(PortID(..), sClose, withSocketsDo, listenOn)
import Data.IORef(IORef, newIORef)
import Control.Monad.Trans(MonadIO(..), MonadTrans(..))
import Control.Monad(liftM)
import Control.Concurrent(forkIO)
import Control.Exception(finally, try, Exception)
import Control.Monad(forever)

import Network.Server.Common.Accept
import Network.Server.Common.HandleLens
import Network.Server.Common.Lens
import Network.Server.Common.Line
import Network.Server.Common.Env
import Network.Server.Common.Ref
import Data.Set(Set)
import qualified Data.Set as S

data Loop v s f a =
  Loop (Env v -> s -> f (a, s))

type IOLoop v s a =
  Loop v s IO a

type IORefLoop v s a =
  IOLoop (IORef v) s a

execLoop ::
  Functor f =>
  Loop v s f a
  -> Env v
  -> s
  -> f a
execLoop (Loop l) e =
  fmap fst . l e

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

etry ::
  Exception e =>
  (Env v -> s -> IO (a, s))
  -> IOLoop v s (Either e (a, s))
etry k =
  Loop (\env s -> fmap (\e -> (e, s)) . try . k env $ s)

server ::
  IO w -- server initialise
  -> (w -> IO v) -- client accepted (pre)
  -> s
  -> IOLoop v s () -- per-client
  -> IO a
server i r t l =
  let hand s w c = forever $
                     do q <- accept' s
                        lSetBuffering q NoBuffering
                        _ <- atomicModifyIORef_ c (S.insert (refL `getL` q))
                        x <- r w
                        forkIO (execLoop l (Env q c x) t)
  in withSocketsDo $ do
       s <- listenOn (PortNumber 6060)
       w <- i
       c <- newIORef S.empty
       hand s w c `finally` sClose s

perClient ::
  s
  -> IOLoop v s x -- client accepted (post)
  -> (String -> IOLoop v s a) -- read line from client
  -> IOLoop v s ()
perClient s q f =
  let lp = do k <- etry undefined -- lGetLine
              case k of Left e -> xprint e
                        Right [] -> lp
                        Right l -> f l >> lp
  in do _ <- q
        lp
