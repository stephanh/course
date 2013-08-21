module Network.Server.Chat.ChatLoop where

import Network.Server.Handle.Line
import Network.Server.Chat.Loop
import Data.Maybe(fromMaybe)
import Data.Foldable(msum)
import Control.Applicative((<$), (<$>))
import Control.Monad.Trans(MonadIO(..))

type ChatLoop a =
  IORefLoop Integer a

data ChatCommand =
  Chat String
  | Incr
  | Unknown String
  deriving (Eq, Show)

incr ::
  ChatLoop Integer
incr =
  do e <- readEnvval
     liftIO $ atomicModifyIORef_ e (+1)

chatLoop ::
  ChatLoop x -- client accepted (post)
  -> (String -> ChatLoop w) -- read line from client
  -> IO a
chatLoop =
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
  -> ChatLoop ()
process (Chat m) =
  allClientsButThis ! "CHAT " ++ m
process Incr =
  do n <- incr
     pPutStrLn ("INCR " ++ show n)
process (Unknown s) =
  pPutStrLn ("UNKNOWN " ++ s)
