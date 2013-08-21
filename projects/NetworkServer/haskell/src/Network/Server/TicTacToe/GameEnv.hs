module Network.Server.TicTacToe.GameEnv where

import Data.TicTacToe
import Network.Server.Common.Lens
import Network.Server.Common.HandleLens(HandleLens(..))
import Network.Server.Common.Accept(Accept)
import Network.Server.Common.Ref(Ref)
import Data.IORef(IORef)
import Data.Set(Set)

type FinishedGames =
  [FinishedBoard]

data GameEnv =
  GameEnv
    Accept
    Unfinished
    (IORef Unfinished)
    (IORef (Set Ref))
    FinishedGames
  deriving Eq

boardrefL ::
  Lens GameEnv (IORef Unfinished)
boardrefL =
  error "todo"
    {-
clientsL ::
  Lens GameEnv (IORef (Set Ref))
clientsL =
  error "todo"
      -}
finishedGamesL ::
  Lens GameEnv FinishedGames
finishedGamesL =
  error "todo"

instance HandleLens GameEnv where
  handleL =
    error "todo"
