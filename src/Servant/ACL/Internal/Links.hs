{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

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
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as KMap
#else
import qualified Data.HashMap.Strict as HMap
#endif
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as T
import GHC.Exts (IsList, IsString)
import GHC.Generics (Generic)
import Network.URI (URI, parseURI)
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

instance FromJSON LinkObject where
  parseJSON = fmap LinkObject . uriFromJSON
   where
    uriFromJSON = withText "URI" $ maybe (fail "not a URI") pure . parseURI . T.unpack


instance ToJSON LinkObject where
  toJSON (LinkObject h) = uriToJSON h
   where
     uriToJSON = String . T.pack . show

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
#if MIN_VERSION_aeson(2,0,0)
    Object o -> Object $ KMap.insert "_links" (toJSON $ _links l) o
#else
    Object o -> Object $ HMap.insert "_links" (toJSON $ _links l) o
#endif
    v ->
      object
        [ "_links" .= toJSON (_links l),
          "value" .= v
        ]

instance FromJSON a => FromJSON (Linked a) where
  parseJSON = withObject "Linked" $ \o -> do
#if MIN_VERSION_aeson(2,0,0)
    val <- parseJSON (Object $ KMap.delete "_links" o)
#else
    val <- parseJSON (Object $ HMap.delete "_links" o)
#endif
    l <- o .: "_links"
    pure $ Linked {
      _links = l,
      value = val
    }
