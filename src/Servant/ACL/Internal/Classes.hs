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

-- | Serve a hoisted generic servant server with ACL.
serveTWithACL ::
  forall routes m.
  ( GenericServant routes AsApi,
    GenericServant routes (AsServerT m),
    GenericServant routes (AsACLT m),
    AndThen (ToServant routes (AsACLT m)) (ServerT (ToServant routes AsApi) m),
    ToServant routes (AsServerT m) ~ ServerT (ToServant routes AsApi) m,
    HasServer (ToServantApi routes) '[ErrorFormatters]
  ) =>
  Proxy routes ->
  (forall a. m a -> Handler a) ->
  routes (AsACLT m) ->
  routes (AsServerT m) ->
  Application
serveTWithACL api nat acl server =
  serveTWithACLAndContext api nat acl server (defaultErrorFormatters :. EmptyContext)

-- | Serve a hoisted generic servant server, with ACL and `Context`.
serveTWithACLAndContext ::
  forall routes m ctx.
  ( GenericServant routes AsApi,
    GenericServant routes (AsServerT m),
    GenericServant routes (AsACLT m),
    HasContextEntry (ctx .++ DefaultErrorFormatters) ErrorFormatters,
    AndThen (ToServant routes (AsACLT m)) (ServerT (ToServant routes AsApi) m),
    ToServant routes (AsServerT m) ~ ServerT (ToServant routes AsApi) m,
    HasServer (ToServantApi routes) ctx
  ) =>
  Proxy routes ->
  (forall a. m a -> Handler a) ->
  routes (AsACLT m) ->
  routes (AsServerT m) ->
  Context ctx ->
  Application
serveTWithACLAndContext api nat acl server ctx =
  serveWithContext
    (genericApi api)
    ctx
    ( hoistServerWithContext (genericApi api) contextP nat $
        toServant acl `andThen` toServant server
    )
  where
    contextP :: Proxy ctx
    contextP = Proxy

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

-- I would have thought `m a` is already more general than `(x -> a)`, but
-- apparently we need to do the `ma ~ m a` trick.
instance {-# OVERLAPPABLE #-} (ma ~ m a, mb ~ m b, Applicative m) => AndThen ma mb where
  andThen = (*>)

instance {-# OVERLAPPING #-} (AndThen a b) => AndThen (x -> a) (x -> b) where
  andThen f g = \x -> f x `andThen` g x

instance (AndThen a0 b0, AndThen a1 b1) => AndThen (a0 :<|> a1) (b0 :<|> b1) where
  andThen (a0 :<|> a1) (b0 :<|> b1) = (a0 `andThen` b0 :<|> a1 `andThen` b1)
