module Network.Server.Chat.Chat where

import Network.Server.Common.Env
import Network.Server.Common.Line
import Network.Server.Chat.Loop
import Data.Maybe(fromMaybe)
import Data.Foldable(msum)
import Control.Applicative((<$), (<$>))
import Control.Monad.Trans(MonadIO(..))

type Chat a =
  IORefLoop Integer a

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
