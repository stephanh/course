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
  Lens
    (\(Env _ s a) x -> Env x s a)
    (\(Env x _ _) -> x)

clientsL ::
  Lens (Env a) (IORef (Set Ref))
clientsL =
  Lens
    (\(Env x _ a) s -> Env x s a)
    (\(Env _ s _) -> s)

envvalL ::
  Lens (Env a) a
envvalL =
  Lens
    (\(Env x s _) a -> Env x s a)
    (\(Env _ _ a) -> a)

instance HandleLens (Env a) where
  handleL =
    acceptL .@ handleL
