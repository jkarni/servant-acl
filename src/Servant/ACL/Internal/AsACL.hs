{-# LANGUAGE UndecidableInstances #-}

module Servant.ACL.Internal.AsACL where

import Control.Monad
import Servant
import Servant.API.Generic
import Servant.Server.Generic

data WithACL m a
  = forall b.
    WithACL
      { aclCheck :: m b,
        handler :: b -> m a
      }

runWithACL :: Monad m => WithACL m a -> m a
runWithACL (WithACL check handler) = check >>= handler

runJustACL :: (Functor m) => WithACL m a -> m ()
runJustACL (WithACL check _) = void check

-- | Make a `WithACL` that has an empty ACL check
noACL :: Applicative m => m a -> WithACL m a
noACL h = withACL_ h (pure ())

-- | Construct a `WithACL`. The second argument is the check.
withACL :: (b -> m a) -> m b -> WithACL m a
withACL handler acl = WithACL acl handler

-- | Like `withACL`, but specialized for the case when the check is `m ()`, and
-- so the handler doesn't need the extra argument.
withACL_ :: m a -> m () -> WithACL m a
withACL_ handler acl = withACL (const handler) acl
