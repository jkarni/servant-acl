{-# LANGUAGE UndecidableInstances #-}

module Servant.ACL.Internal.Classes where

import Servant
import Servant.API.Generic
import Servant.Server.Internal

-- | A datatype `AsACL m` is exactly the same as `AsServerT m`, except that the
-- return value under the m monad is always ().
--
-- E.g, if the `AsServerT Handler` of a datatype is
--
-- > Foo -> Bar -> Handler Baz
--
-- The `AsACL Handler` of that datatype will be:
--
-- > Foo -> Bar -> Handler ()
data AsACL (m :: * -> *)

instance GenericMode (AsACL m) where
  type AsACL m :- api = ACLOfT api m

type family ACLOfT (api :: *) (m :: * -> *) where
  ACLOfT (Verb method status cts a) m = m ()
  ACLOfT api m = ServerT api m

class AndThen a b where
  andThen :: a -> b -> b

instance Applicative m => AndThen (m a) (m b) where
  andThen = (*>)

instance (AndThen a0 b0, AndThen a1 b1) => AndThen (a0 :<|> a1) (b0 :<|> b1) where
  andThen (a0 :<|> a1) (b0 :<|> b1) = (a0 `andThen` b0 :<|> a1 `andThen` b1)

instance (AndThen a b) => AndThen (x -> a) (x -> b) where
  andThen f g = \x -> f x `andThen` g x
