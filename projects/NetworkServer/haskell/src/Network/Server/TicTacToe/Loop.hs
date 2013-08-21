module Network.Server.TicTacToe.Loop where

import Network.Server.Common.Env

data Loop v s f a =
  Loop (Env v -> s -> f (a, s))
