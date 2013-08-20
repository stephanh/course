module Network.Server.Chat.Chat where

import Network.Server.Chat.ChatLoop
import Network.Server.Handle.Loop

main ::
  IO a
main =
  chatLoop (readIOEnvval >>= pPutStrLn . show) (process . chatCommand)
