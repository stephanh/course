module Network.Server.Chat.Chat where

import Network.Server.Handle.Loop
import Data.Char(isSpace, toLower)
import Data.Maybe(fromMaybe)
import Data.Foldable(msum)
import Control.Applicative((<$), (<$>))
import Data.Function(on)
import Control.Monad.Trans(MonadIO(..))

type ChatLoop a =
  IORefLoop Integer a

data ChatCommand =
  Chat String
  | Incr
  | Unknown String
  deriving (Eq, Show)

incr ::
  ChatLoop ()
incr =
  do e <- readEnvval
     liftIO $ atomicModifyIORef_ e (+1)

chatLoop ::
  ChatLoop x -- client accepted (post)
  -> (String -> ChatLoop w) -- read line from client
  -> IO a
chatLoop =
  iorefLoop 0

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
  do incr
     undefined
process (Unknown s) =
  pPutStrLn ("UNKNOWN " ++ s)

wibble =
  chatLoop (readIOEnvval >>= pPutStrLn . show) (process . chatCommand)

-- |
--
-- >>> trimPrefixThen "ABC" "AB"
-- Nothing
--
-- >>> trimPrefixThen "ABC" "ABC"
-- Just ""
--
-- >>> trimPrefixThen "ABC" "ABCDEF"
-- Just "DEF"
--
-- >>> trimPrefixThen "ABC" "Ab"
-- Nothing
--
-- >>> trimPrefixThen "ABC" "Abc"
-- Just ""
--
-- >>> trimPrefixThen "ABC" "Abcdef"
-- Just "def"
--
-- >>> trimPrefixThen "ABC" "Abcdef   ghi  "
-- Just "def   ghi"
trimPrefixThen ::
  String
  -> String
  -> Maybe String
trimPrefixThen l z =
  reverse . dropWhile isSpace . reverse . dropWhile isSpace <$> prefixThen ((==) `on` toLower) l z

-- |
--
-- >>> prefixThen (==) "ABC" "AB"
-- Nothing
--
-- >>> prefixThen (==) "ABC" "ABC"
-- Just ""
--
-- >>> prefixThen (==) "ABC" "ABCDEF"
-- Just "DEF"
prefixThen ::
  (a -> a -> Bool)
  -> [a]
  -> [a]
  -> Maybe [a]
prefixThen _ [] r =
  Just r
prefixThen _ _ [] =
  Nothing
prefixThen e (a:b) (c:d) =
  if e a c
    then
      prefixThen e b d
    else
      Nothing
