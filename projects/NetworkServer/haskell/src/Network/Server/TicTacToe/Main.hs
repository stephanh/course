module Main
(
  main
) where

import Network.Server.TicTacToe.Game

main ::
  IO ()
main =
  server game