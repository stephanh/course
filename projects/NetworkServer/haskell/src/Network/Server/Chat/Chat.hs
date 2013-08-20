module Network.Server.Chat.Chat where

import Network.Server.Handle.Env
import Network.Server.Handle.Loop
import Data.IORef(IORef, newIORef, readIORef)

type ChatEnv =
  Env (IORef Integer)

type ChatLoop a =
  IOLoop Integer a

chatLoop ::
  ChatLoop x -- client accepted (post)
  -> (String -> ChatLoop w) -- read line from client
  -> IO a
chatLoop =
  iorefLoop 0



{-

server ::
  v
  -> Loop v IO ()
  -> IO a
server i (Loop f) =
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

perClient ::
  Loop v IO x
  -> (String -> Loop v IO a)
  -> Loop v IO ()
perClient q f =
  let lp = do k <- etry lGetLine
              case k of Left e -> xprint e
                        Right [] -> lp
                        Right l -> f l >> lp
  in do _ <- q
        lp

loop ::
  v
  -> Loop v IO x
  -> (String -> Loop v IO w)
  -> IO a
loop i q f =
  server i (perClient q f)
 -}