module Network.Server.Handle.Env where

import Network.Server.Handle.Accept
import Network.Server.Handle.Ref
import Network.Server.Handle.HandleLens
import Network.Server.Handle.Lens
import Data.IORef(IORef)
import Data.Set(Set)

data Env a =
  Env
    Accept
    (IORef (Set Ref))
    a
  deriving Eq

acceptL ::
  Lens (Env a) Accept
acceptL =
  error "todo"

clientsL ::
  Lens (Env a) (IORef (Set Ref))
clientsL =
  error "todo"

envvalL ::
  Lens (Env a) a
envvalL =
  error "todo"

instance HandleLens (Env a) where
  handleL =
    error "todo"