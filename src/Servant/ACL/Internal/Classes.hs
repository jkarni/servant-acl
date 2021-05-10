{-# LANGUAGE UndecidableInstances #-}

module Servant.ACL.Internal.Classes where

import Servant
import Servant.API.Generic
import Servant.Server.Generic

-- | Serve a generic servant server, with ACL.
serveWithACL ::
  ( GenericServant routes AsApi,
    GenericServant routes AsServer,
    GenericServant routes AsACL,
    AndThen (ToServant routes AsACL) (Server (ToServant routes AsApi)),
    ToServant routes AsServer ~ Server (ToServant routes AsApi),
    HasServer (ToServantApi routes) '[]
  ) =>
  Proxy routes ->
  routes AsACL ->
  routes AsServer ->
  Application
serveWithACL api acl server =
  serve
    (genericApi api)
    (toServant acl `andThen` genericServer server)

-- | A datatype `AsACLT m` is exactly the same as `AsServerT m`, except that the
-- return value under the m monad is always ().
--
-- E.g, if the `AsServerT Handler` of a datatype is
--
-- > Foo -> Bar -> Handler Baz
--
-- The `AsACL Handler` of that datatype will be:
--
-- > Foo -> Bar -> Handler ()
data AsACLT (m :: * -> *)

type AsACL = AsACLT Handler

instance GenericMode (AsACLT m) where
  type AsACLT m :- api = VoidReturn (ServerT api m)

type family VoidReturn a where
  VoidReturn (a -> b) = a -> VoidReturn b
  VoidReturn (m a) = m ()

class AndThen a b where
  andThen :: a -> b -> b

instance {-# OVERLAPPABLE #-} Applicative m => AndThen (m a) (m b) where
  andThen = (*>)

instance {-# OVERLAPPING #-} (AndThen a b) => AndThen (x -> a) (x -> b) where
  andThen f g = \x -> f x `andThen` g x

instance (AndThen a0 b0, AndThen a1 b1) => AndThen (a0 :<|> a1) (b0 :<|> b1) where
  andThen (a0 :<|> a1) (b0 :<|> b1) = (a0 `andThen` b0 :<|> a1 `andThen` b1)
