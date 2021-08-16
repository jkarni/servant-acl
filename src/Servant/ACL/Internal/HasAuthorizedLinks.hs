{-# LANGUAGE UndecidableInstances #-}

-- A lot of this comes from Servant.Utils.Links and so is (c) 2014-2016 Zalora
-- South East Asia Pte Ltd, 2016-2021 Servant Contributors

module Servant.ACL.Internal.HasAuthorizedLinks where

import Data.List (foldl', intercalate)
import Data.Proxy
import qualified Data.Text as Text
import GHC.TypeLits (KnownSymbol, symbolVal)
import Network.URI (escapeURIString, isUnreserved)
import Servant (ServerT)
import Servant.ACL.Internal.AsACL
import qualified Servant.Auth as SAuth
import qualified Servant.Auth.Server as SAuth ()
import Servant.API hiding (Link)
import Servant.API.Generic
import Servant.API.Modifiers (FoldLenient, FoldRequired, RequestArgument)
import Servant.Server.Experimental.Auth ()
import Servant.Server.Generic
import Servant.Multipart

{-

This would be a nicer interface. E.g.:
  linkIfAuthorized wholeAPI (\x -> x ^. subAPI subArg ^. subSubAPI )

linkIfAuthorized ::
  forall routes m check x.
  ( Applicative m,
    HasAuthorizedLink check,
    IsElem check (ToServantApi routes)
  ) =>
  routes (AsServerT (WithACL m)) ->
  (forall m. routes (AsServerT m) -> x) ->
  m (MkAuthorizedLink check Link)
linkIfAuthorized routes getter = do
  pure $
    toAuthorizedLink
      id
      (Proxy :: Proxy check)
      (Link mempty mempty mempty)
-}

allAuthorizedLinks ::
  (Monad m, HasAuthorizedLink api) =>
  Proxy api ->
  Proxy m ->
  ServerT api (WithACL m) ->
  MkAuthorizedLink api (m Link)
allAuthorizedLinks api m server =
  toAuthorizedLink id api server m (Link mempty mempty mempty)

allAuthorizedFieldLinks ::
  forall m routes.
  ( Monad m,
    ToServant routes (AsServerT (WithACL m))
      ~ ServerT (ToServantApi routes) (WithACL m),
    GenericServant routes (AsServerT (WithACL m)),
    GenericServant routes AsApi,
    GenericServant routes (AsAuthorizedLink (m Link)),
    ToServant routes (AsAuthorizedLink (m Link))
      ~ MkAuthorizedLink (ToServantApi routes) (m Link),
    HasAuthorizedLink (ToServantApi routes)
  ) =>
  routes (AsServerT (WithACL m)) ->
  routes (AsAuthorizedLink (m Link))
allAuthorizedFieldLinks server =
  fromServant $
    allAuthorizedLinks
      (Proxy :: Proxy (ToServantApi routes))
      (Proxy :: Proxy m)
      (toServant server)

data AsAuthorizedLink (a :: *)

instance GenericMode (AsAuthorizedLink a) where
  type (AsAuthorizedLink a) :- api = MkAuthorizedLink api a

-- * HasAuthorizedLink

-- | Like HasLink, but uses the same arguments as HasACL.
class HasAuthorizedLink endpoint where
  type MkAuthorizedLink endpoint a
  toAuthorizedLink ::
    forall a m.
    Monad m =>
    (Link -> a) ->
    Proxy endpoint ->
    ServerT endpoint (WithACL m) ->
    Proxy m ->
    Link ->
    MkAuthorizedLink endpoint (m a)

{-
-- | Helper for implementing 'toAuthorizedLink' for combinators not affecting link
-- structure. Different than the simpleToLink in Servant.Links in that it uses
-- a const rather than dropping the argument.
simpleToLink' ::
  forall sub a combinator x.
  ( HasAuthorizedLink sub,
    (x -> MkAuthorizedLink sub a) ~ MkAuthorizedLink (combinator :> sub) a
  ) =>
  (Link -> a) ->
  Proxy (combinator :> sub) ->
  Link ->
  MkAuthorizedLink (combinator :> sub) a
simpleToLink' toA _ l = const $ toAuthorizedLink toA (Proxy :: Proxy sub) l

simpleToLink ::
  forall sub a combinator.
  ( HasAuthorizedLink sub,
    MkAuthorizedLink sub a ~ MkAuthorizedLink (combinator :> sub) a
  ) =>
  Proxy sub ->
  (Link -> a) ->
  Proxy (combinator :> sub) ->
  Link ->
  MkAuthorizedLink (combinator :> sub) a
simpleToLink toA _ l = toAuthorizedLink toA (Proxy :: Proxy sub) l
-}

-- Naked symbol instance
instance (KnownSymbol sym, HasAuthorizedLink sub) => HasAuthorizedLink (sym :> sub) where
  type MkAuthorizedLink (sym :> sub) a = MkAuthorizedLink sub a
  toAuthorizedLink toA _pEp servEp pm =
    toAuthorizedLink toA (Proxy :: Proxy sub) servEp pm . addSegment (escaped seg)
    where
      seg = symbolVal (Proxy :: Proxy sym)

-- Verb (terminal) instances
instance HasAuthorizedLink (Verb m s ct a) where
  type MkAuthorizedLink (Verb m s ct a) r = r
  toAuthorizedLink toA _pEp servEp _pm link = runJustACL servEp >> pure (toA link)

-- Capture
instance
  (ToHttpApiData v, SBoolI (FoldLenient mods), HasAuthorizedLink sub) =>
  HasAuthorizedLink (Capture' mods sym v :> sub)
  where
  type
    MkAuthorizedLink (Capture' mods sym v :> sub) a =
      If (FoldLenient mods) (Either String v) v -> MkAuthorizedLink sub a
  toAuthorizedLink toA _pEp servEp pm link = \arg ->
    let seg = case sbool :: SBool (FoldLenient mods) of
          STrue -> case arg of
            Left s -> Escaped s
            Right v -> escaped . Text.unpack $ toUrlPiece v
          SFalse -> escaped . Text.unpack $ toUrlPiece arg
     in toAuthorizedLink toA (Proxy :: Proxy sub) (servEp arg) pm $
          addSegment seg link

-- QueryParam instances
instance
  ( KnownSymbol sym,
    ToHttpApiData v,
    HasAuthorizedLink sub,
    SBoolI (FoldLenient mods),
    SBoolI (FoldRequired mods)
  ) =>
  HasAuthorizedLink (QueryParam' mods sym v :> sub)
  where
  type MkAuthorizedLink (QueryParam' mods sym v :> sub) a = RequestArgument mods v -> MkAuthorizedLink sub a
  toAuthorizedLink toA _pEp servEp pm link = \arg ->
    toAuthorizedLink toA (Proxy :: Proxy sub) (servEp arg) pm $
      case (sbool :: SBool (FoldRequired mods), sbool :: SBool (FoldLenient mods)) of
        (STrue, STrue) -> (addQueryParam . SingleParam k . toQueryParam) arg link
        (STrue, SFalse) -> (addQueryParam . SingleParam k . toQueryParam) arg link
        (SFalse, STrue) -> maybe id (addQueryParam . SingleParam k . toQueryParam) arg link
        (SFalse, SFalse) -> maybe id (addQueryParam . SingleParam k . toQueryParam) arg link
    where
      k :: String
      k = symbolVal (Proxy :: Proxy sym)

instance (HasAuthorizedLink a, HasAuthorizedLink b) => HasAuthorizedLink (a :<|> b) where
  type MkAuthorizedLink (a :<|> b) r = MkAuthorizedLink a r :<|> MkAuthorizedLink b r
  toAuthorizedLink toA _pEp (servL :<|> servR) pm link =
    toAuthorizedLink toA (Proxy :: Proxy a) servL pm link
      :<|> toAuthorizedLink toA (Proxy :: Proxy b) servR pm link

instance
  (KnownSymbol sym, ToHttpApiData v, HasAuthorizedLink sub) =>
  HasAuthorizedLink (QueryParams sym v :> sub)
  where
  type MkAuthorizedLink (QueryParams sym v :> sub) a = [v] -> MkAuthorizedLink sub a
  toAuthorizedLink toA _pEp servEp pm link = \args ->
    toAuthorizedLink toA (Proxy :: Proxy sub) (servEp args) pm $
      foldl' (\l' v -> addQueryParam (ArrayElemParam k (toQueryParam v)) l') link args
    where
      k = symbolVal (Proxy :: Proxy sym)

instance HasAuthorizedLink sub => HasAuthorizedLink (ReqBody' mods ct a :> sub) where
  type
    MkAuthorizedLink (ReqBody' mods ct a :> sub) r =
      If (FoldLenient mods) (Either String a) a -> MkAuthorizedLink sub r
  toAuthorizedLink toA _pEp servEp pm link = \arg ->
    toAuthorizedLink toA (Proxy :: Proxy sub) (servEp arg) pm link

instance HasAuthorizedLink sub => HasAuthorizedLink (BasicAuth scope a :> sub) where
  type
    MkAuthorizedLink (BasicAuth scope a :> sub) r =
      a -> MkAuthorizedLink sub r
  toAuthorizedLink toA _pEp servEp pm link = \arg ->
    toAuthorizedLink toA (Proxy :: Proxy sub) (servEp arg) pm link

instance HasAuthorizedLink sub => HasAuthorizedLink (SAuth.Auth auths a :> sub) where
  type
    MkAuthorizedLink (SAuth.Auth auths a :> sub) r =
      a -> MkAuthorizedLink sub r
  toAuthorizedLink toA _pEp servEp pm link = \arg ->
    toAuthorizedLink toA (Proxy :: Proxy sub) (servEp arg) pm link

instance HasAuthorizedLink sub => HasAuthorizedLink (MultipartForm t a :> sub) where
  type
    MkAuthorizedLink (MultipartForm t a :> sub) r =
      a -> MkAuthorizedLink sub r
  toAuthorizedLink toA _pEp servEp pm link = \arg ->
    toAuthorizedLink toA (Proxy :: Proxy sub) (servEp arg) pm link

-- This should be rethought. Likely it should take an argument that is the URI.
instance HasAuthorizedLink Raw where
  type MkAuthorizedLink Raw r = r
  toAuthorizedLink toA _ _ _ = pure . toA

{-
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

-- Misc instances

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
-}

-- Copying the datatype over from servant
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

linkURI :: Link -> URI
linkURI = linkURI' LinkArrayElementBracket

-- | How to encode array query elements.
data LinkArrayElementStyle
  = -- | @foo[]=1&foo[]=2@
    LinkArrayElementBracket
  | -- | @foo=1&foo=2@
    LinkArrayElementPlain
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Configurable 'linkURI'.
--
-- >>> type API = "sum" :> QueryParams "x" Int :> Get '[JSON] Int
-- >>> linkURI' LinkArrayElementBracket $ safeLink (Proxy :: Proxy API) (Proxy :: Proxy API) [1, 2, 3]
-- sum?x[]=1&x[]=2&x[]=3
--
-- >>> linkURI' LinkArrayElementPlain $ safeLink (Proxy :: Proxy API) (Proxy :: Proxy API) [1, 2, 3]
-- sum?x=1&x=2&x=3
linkURI' :: LinkArrayElementStyle -> Link -> URI
linkURI' addBrackets (Link segments q_params mfragment) =
  URI
    mempty -- No scheme (relative)
    Nothing -- Or authority (relative)
    (intercalate "/" $ map getEscaped segments)
    (makeQueries q_params)
    (makeFragment mfragment)
  where
    makeQueries :: [Param] -> String
    makeQueries [] = ""
    makeQueries xs =
      "?" <> intercalate "&" (fmap makeQuery xs)
    makeQuery :: Param -> String
    makeQuery (ArrayElemParam k v) = escape k <> style <> escape (Text.unpack v)
    makeQuery (SingleParam k v) = escape k <> "=" <> escape (Text.unpack v)
    makeQuery (FlagParam k) = escape k
    makeFragment :: Fragment' -> String
    makeFragment Nothing = ""
    makeFragment (Just fr) = "#" <> escape fr
    style = case addBrackets of
      LinkArrayElementBracket -> "[]="
      LinkArrayElementPlain -> "="

escape :: String -> String
escape = escapeURIString isUnreserved
