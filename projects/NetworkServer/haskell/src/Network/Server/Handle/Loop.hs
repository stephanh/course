module Network.Server.Handle.Loop where

import Network(PortID(..), sClose, withSocketsDo, listenOn)
import System.IO(BufferMode(..))
import Data.IORef(IORef, readIORef, newIORef, atomicModifyIORef)
import Control.Concurrent(forkIO)
import Control.Exception(finally, try, IOException, Exception)
import Control.Monad(forever)
import Control.Monad.Trans(MonadTrans(..), MonadIO(..))

import Network.Server.Handle.Accept
import Network.Server.Handle.HandleLens
import Network.Server.Handle.Lens
import Network.Server.Handle.Env
import qualified Data.Set as S

data Loop v f a =
  Loop (Env v -> f a)

instance Functor f => Functor (Loop v f) where
  fmap f (Loop k) =
    Loop (fmap f . k)

instance Monad f => Monad (Loop v f) where
  return =
    Loop . return . return
  Loop k >>= f =
    Loop (\v -> k v >>= \a ->
      let Loop l = f a
      in l v)

instance MonadTrans (Loop v) where
  lift =
    Loop . const

instance MonadIO f => MonadIO (Loop v f) where
  liftIO =
    lift . liftIO

xprint ::
  IOException
  -> Loop v IO ()
xprint =
  liftIO . print

etry ::
  Exception e =>
  (Env v -> IO a)
  -> Loop v IO (Either e a)
etry k =
  Loop $ try . k

server ::
  Loop v IO ()
  -> v
  -> IO a
server (Loop f) i =
  let hand s w c = forever $
                     do q <- accept' s
                        lSetBuffering q NoBuffering
                        _ <- atomicModifyIORef_ c (S.insert (refL `getL` q))
                        x <- readIORef w
                        forkIO (f (Env q c x))
  in withSocketsDo $ do
       s <- listenOn (PortNumber 6060)
       w <- newIORef i
       c <- newIORef S.empty
       hand s w c `finally` sClose s

client ::
  Loop v IO x
  -> (String -> Loop v IO a)
  -> Loop v IO ()
client q f =
  let loop = do k <- etry lGetLine
                case k of Left e -> xprint e
                          Right [] -> loop
                          Right l -> f l >> loop
  in do _ <- q
        loop

atomicModifyIORef_ ::
  IORef a
  -> (a -> a)
  -> IO ()
atomicModifyIORef_ r f =
  atomicModifyIORef r (\a -> (f a, ()))
