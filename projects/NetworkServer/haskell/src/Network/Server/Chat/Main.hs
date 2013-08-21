module Network.Server.Chat.Main where

import Network.Server.Chat.Chat
import Network.Server.Handle.Loop

main ::
  IO a
main =
  chat (readIOEnvval >>= pPutStrLn . show) (process . chatCommand)
