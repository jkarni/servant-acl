{-# LANGUAGE UndecidableInstances #-}

-- | This module provides some helpers for adding links between your resources.
-- Links are added only when actions are permitted, according to their ACL
-- handlers.
--
-- We follow the Hypermedia Application Language (HAL) format [0].
--
-- [0] https://datatracker.ietf.org/doc/html/draft-kelly-json-hal
module Servant.ACL.Internal.Links where

import Control.Monad.Except
import Data.Aeson
import qualified Data.HashMap.Strict as HMap
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as T
import GHC.Exts (IsList, IsString)
import GHC.Generics (Generic)
import Network.URI (URI)
import Network.URI.JSON ()
import Servant.ACL.Internal.HasAuthorizedLinks

newtype Rel = Rel {getRel :: T.Text}
  deriving stock (Eq, Show, Ord, Read, Generic)
  deriving newtype (IsString, ToJSONKey, FromJSONKey)

-- | See  <https://datatracker.ietf.org/doc/html/draft-kelly-json-hal#section-5 Section 5>
-- for a description of this.
data LinkObject
  = LinkObject
      {href :: URI}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

linkObjectFromLink :: Link -> LinkObject
linkObjectFromLink l = LinkObject $ linkURI l

newtype HalLinks = HalLinks {getHalLinks :: Map.Map Rel LinkObject}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid, IsList, ToJSON, FromJSON)

-- | A @Linked X@ is an @X@ ornamented with Hal Links (and which serializes to
-- and from JSON as the spec prescribes).
data Linked a
  = Linked
      { value :: a,
        _links :: HalLinks
      }
  deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)

withLinks :: MonadError e m => a -> [(Rel, m Link)] -> m (Linked a)
withLinks o links = do
  maybeLinks <-
    sequence
      [ ((\a -> Just (rel, linkObjectFromLink a)) <$> linkAction)
          `catchError` const (pure Nothing)
        | (rel, linkAction) <- links
      ]
  let caughtLinks =
        HalLinks . Map.fromList $ catMaybes maybeLinks
  pure $ Linked o caughtLinks

instance ToJSON a => ToJSON (Linked a) where
  toJSON l = case toJSON (value l) of
    Object o -> Object $ HMap.insert "_links" (toJSON $ _links l) o
    v ->
      object
        [ "_links" .= (toJSON $ _links l),
          "value" .= v
        ]
