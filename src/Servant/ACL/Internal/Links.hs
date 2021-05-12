{-# LANGUAGE UndecidableInstances #-}

-- A lot of this comes from Servant.Utils.Links and so is (c) 2014-2016 Zalora
-- South East Asia Pte Ltd, 2016-2021 Servant Contributors

module Servant.ACL.Internal.Links where

import Data.List (foldl')
import Data.Proxy
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import GHC.TypeLits (KnownSymbol, symbolVal)
import Network.URI (URI (..), escapeURIString, isUnreserved)
import Servant.ACL.Internal.Classes
import Servant.API hiding (Link)
import Servant.API.Generic
import Servant.API.Modifiers (FoldRequired)
import Servant.Server.Experimental.Auth

linkIfAuthorized ::
  forall routes m check.
  ( Applicative m,
    AndThen check (m (MkAuthorizedLink check Link)),
    HasAuthorizedLink check,
    IsElem check (ToServantApi routes)
  ) =>
  routes (AsACLT m) ->
  (routes (AsACLT m) -> check) ->
  m (MkAuthorizedLink check Link)
linkIfAuthorized routes getter = do
  getter routes
    `andThen` ( pure $
                  toAuthorizedLink
                    id
                    (Proxy :: Proxy check)
                    (Link mempty mempty mempty)
              )

-- * HasAuthorizedLink

-- | Like HasLink, but uses the same arguments as HasACL.
class HasAuthorizedLink endpoint where
  type MkAuthorizedLink endpoint a
  toAuthorizedLink ::
    (Link -> a) ->
    Proxy endpoint ->
    Link ->
    MkAuthorizedLink endpoint a

-- | Helper for implementing 'toAuthorizedLink' for combinators not affecting link
-- structure. Different than the simpleToLink in Servant.Links in that it uses
-- a const rather than dropping the argument.
simpleToLink' ::
  forall sub a combinator x.
  ( HasAuthorizedLink sub,
    (x -> MkAuthorizedLink sub a) ~ MkAuthorizedLink (combinator :> sub) a
  ) =>
  Proxy sub ->
  (Link -> a) ->
  Proxy (combinator :> sub) ->
  Link ->
  MkAuthorizedLink (combinator :> sub) a
simpleToLink' _ toA _ l = const $ toAuthorizedLink toA (Proxy :: Proxy sub) l

simpleToLink ::
  forall sub a combinator x.
  ( HasAuthorizedLink sub,
    MkAuthorizedLink sub a ~ MkAuthorizedLink (combinator :> sub) a
  ) =>
  Proxy sub ->
  (Link -> a) ->
  Proxy (combinator :> sub) ->
  Link ->
  MkAuthorizedLink (combinator :> sub) a
simpleToLink _ toA _ l = toAuthorizedLink toA (Proxy :: Proxy sub) l

-- Remember kids, always export everything - create internal modules if need be.
--

data Link
  = Link
      { _segments :: [Escaped],
        _queryParams :: [Param],
        _fragment :: Fragment'
      }
  deriving (Show)

-- | Query parameter.
data Param
  = SingleParam String Text.Text
  | ArrayElemParam String Text.Text
  | FlagParam String
  deriving (Show)

newtype Escaped = Escaped String

type Fragment' = Maybe String

instance Show Escaped where
  showsPrec d (Escaped s) = showsPrec d s
  show (Escaped s) = show s

escaped :: String -> Escaped
escaped = Escaped . escapeURIString isUnreserved

getEscaped :: Escaped -> String
getEscaped (Escaped s) = s

linkSegments :: Link -> [String]
linkSegments = map getEscaped . _segments

addSegment :: Escaped -> Link -> Link
addSegment seg l = l {_segments = _segments l <> [seg]}

addQueryParam :: Param -> Link -> Link
addQueryParam qp l =
  l {_queryParams = _queryParams l <> [qp]}

addFragment :: Fragment' -> Link -> Link
addFragment fr l = l {_fragment = fr}

-- From here on we copy the HasLink code, modifying some places so that
-- parameters are still used.
-- It is quite inelegant.

-- Naked symbol instance
instance (KnownSymbol sym, HasAuthorizedLink sub) => HasAuthorizedLink (sym :> sub) where
  type MkAuthorizedLink (sym :> sub) a = MkAuthorizedLink sub a
  toAuthorizedLink toA _ =
    toAuthorizedLink toA (Proxy :: Proxy sub) . addSegment (escaped seg)
    where
      seg = symbolVal (Proxy :: Proxy sym)

-- QueryParam instances
instance
  (KnownSymbol sym, ToHttpApiData v, HasAuthorizedLink sub, SBoolI (FoldRequired mods)) =>
  HasAuthorizedLink (QueryParam' mods sym v :> sub)
  where
  type MkAuthorizedLink (QueryParam' mods sym v :> sub) a = If (FoldRequired mods) v (Maybe v) -> MkAuthorizedLink sub a
  toAuthorizedLink toA _ l mv =
    toAuthorizedLink toA (Proxy :: Proxy sub) $
      case sbool :: SBool (FoldRequired mods) of
        STrue -> (addQueryParam . SingleParam k . toQueryParam) mv l
        SFalse -> maybe id (addQueryParam . SingleParam k . toQueryParam) mv l
    where
      k :: String
      k = symbolVal (Proxy :: Proxy sym)

instance
  (KnownSymbol sym, ToHttpApiData v, HasAuthorizedLink sub) =>
  HasAuthorizedLink (QueryParams sym v :> sub)
  where
  type MkAuthorizedLink (QueryParams sym v :> sub) a = [v] -> MkAuthorizedLink sub a
  toAuthorizedLink toA _ l =
    toAuthorizedLink toA (Proxy :: Proxy sub)
      . foldl' (\l' v -> addQueryParam (ArrayElemParam k (toQueryParam v)) l') l
    where
      k = symbolVal (Proxy :: Proxy sym)

instance
  (KnownSymbol sym, HasAuthorizedLink sub) =>
  HasAuthorizedLink (QueryFlag sym :> sub)
  where
  type MkAuthorizedLink (QueryFlag sym :> sub) a = Bool -> MkAuthorizedLink sub a
  toAuthorizedLink toA _ l False =
    toAuthorizedLink toA (Proxy :: Proxy sub) l
  toAuthorizedLink toA _ l True =
    toAuthorizedLink toA (Proxy :: Proxy sub) $ addQueryParam (FlagParam k) l
    where
      k = symbolVal (Proxy :: Proxy sym)

-- :<|> instance - Generate all links at once
instance (HasAuthorizedLink a, HasAuthorizedLink b) => HasAuthorizedLink (a :<|> b) where
  type MkAuthorizedLink (a :<|> b) r = MkAuthorizedLink a r :<|> MkAuthorizedLink b r
  toAuthorizedLink toA _ l = toAuthorizedLink toA (Proxy :: Proxy a) l :<|> toAuthorizedLink toA (Proxy :: Proxy b) l

-- Misc instances
instance HasAuthorizedLink sub => HasAuthorizedLink (ReqBody' mods ct a :> sub) where
  type MkAuthorizedLink (ReqBody' mods ct a :> sub) r = MkAuthorizedLink sub r
  toAuthorizedLink toA _ = toAuthorizedLink toA (Proxy :: Proxy sub)

instance HasAuthorizedLink sub => HasAuthorizedLink (StreamBody' mods framing ct a :> sub) where
  type MkAuthorizedLink (StreamBody' mods framing ct a :> sub) r = MkAuthorizedLink sub r
  toAuthorizedLink toA _ = toAuthorizedLink toA (Proxy :: Proxy sub)

instance
  (ToHttpApiData v, HasAuthorizedLink sub) =>
  HasAuthorizedLink (Capture' mods sym v :> sub)
  where
  type MkAuthorizedLink (Capture' mods sym v :> sub) a = v -> MkAuthorizedLink sub a
  toAuthorizedLink toA _ l v =
    toAuthorizedLink toA (Proxy :: Proxy sub) $
      addSegment (escaped . Text.unpack $ toUrlPiece v) l

instance
  (ToHttpApiData v, HasAuthorizedLink sub) =>
  HasAuthorizedLink (CaptureAll sym v :> sub)
  where
  type MkAuthorizedLink (CaptureAll sym v :> sub) a = [v] -> MkAuthorizedLink sub a
  toAuthorizedLink toA _ l vs =
    toAuthorizedLink toA (Proxy :: Proxy sub) $
      foldl' (flip $ addSegment . escaped . Text.unpack . toUrlPiece) l vs

instance HasAuthorizedLink sub => HasAuthorizedLink (Header' mods sym (a :: *) :> sub) where
  type MkAuthorizedLink (Header' mods sym a :> sub) r = MkAuthorizedLink sub r
  toAuthorizedLink = simpleToLink (Proxy :: Proxy sub)

instance HasAuthorizedLink sub => HasAuthorizedLink (Vault :> sub) where
  type MkAuthorizedLink (Vault :> sub) a = Vault -> MkAuthorizedLink sub a
  toAuthorizedLink = simpleToLink' (Proxy :: Proxy sub)

instance HasAuthorizedLink sub => HasAuthorizedLink (Description s :> sub) where
  type MkAuthorizedLink (Description s :> sub) a = MkAuthorizedLink sub a
  toAuthorizedLink = simpleToLink (Proxy :: Proxy sub)

instance HasAuthorizedLink sub => HasAuthorizedLink (Summary s :> sub) where
  type MkAuthorizedLink (Summary s :> sub) a = MkAuthorizedLink sub a
  toAuthorizedLink = simpleToLink (Proxy :: Proxy sub)

instance HasAuthorizedLink sub => HasAuthorizedLink (HttpVersion :> sub) where
  type MkAuthorizedLink (HttpVersion :> sub) a = HttpVersion -> MkAuthorizedLink sub a
  toAuthorizedLink = simpleToLink' (Proxy :: Proxy sub)

instance HasAuthorizedLink sub => HasAuthorizedLink (IsSecure :> sub) where
  type MkAuthorizedLink (IsSecure :> sub) a = IsSecure -> MkAuthorizedLink sub a
  toAuthorizedLink = simpleToLink' (Proxy :: Proxy sub)

instance HasAuthorizedLink sub => HasAuthorizedLink (WithNamedContext name context sub) where
  type MkAuthorizedLink (WithNamedContext name context sub) a = MkAuthorizedLink sub a
  toAuthorizedLink toA _ = toAuthorizedLink toA (Proxy :: Proxy sub)

instance HasAuthorizedLink sub => HasAuthorizedLink (RemoteHost :> sub) where
  type MkAuthorizedLink (RemoteHost :> sub) a = RemoteHost -> MkAuthorizedLink sub a
  toAuthorizedLink = simpleToLink' (Proxy :: Proxy sub)

instance HasAuthorizedLink sub => HasAuthorizedLink (BasicAuth realm a :> sub) where
  type MkAuthorizedLink (BasicAuth realm a :> sub) r = a -> MkAuthorizedLink sub r
  toAuthorizedLink = simpleToLink' (Proxy :: Proxy sub)

instance HasAuthorizedLink EmptyAPI where
  type MkAuthorizedLink EmptyAPI a = EmptyAPI
  toAuthorizedLink _ _ _ = EmptyAPI

-- Verb (terminal) instances
instance HasAuthorizedLink (Verb m s ct a) where
  type MkAuthorizedLink (Verb m s ct a) r = r
  toAuthorizedLink toA _ = toA

instance HasAuthorizedLink (NoContentVerb m) where
  type MkAuthorizedLink (NoContentVerb m) r = r
  toAuthorizedLink toA _ = toA

instance HasAuthorizedLink Raw where
  type MkAuthorizedLink Raw a = a
  toAuthorizedLink toA _ = toA

instance HasAuthorizedLink (Stream m status fr ct a) where
  type MkAuthorizedLink (Stream m status fr ct a) r = r
  toAuthorizedLink toA _ = toA

-- AuthProtext instances
instance HasAuthorizedLink sub => HasAuthorizedLink (AuthProtect tag :> sub) where
  type
    MkAuthorizedLink (AuthProtect tag :> sub) a =
      AuthServerData (AuthProtect tag) -> MkAuthorizedLink sub a
  toAuthorizedLink = simpleToLink' (Proxy :: Proxy sub)

instance
  (HasAuthorizedLink sub, ToHttpApiData v) =>
  HasAuthorizedLink (Fragment v :> sub)
  where
  type MkAuthorizedLink (Fragment v :> sub) a = v -> MkAuthorizedLink sub a
  toAuthorizedLink toA _ l mv =
    toAuthorizedLink toA (Proxy :: Proxy sub) $
      addFragment ((Just . Text.unpack . toQueryParam) mv) l
