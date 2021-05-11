-- A lot of this comes from Servant.Utils.Links and so is (c) 2014-2016 Zalora
-- South East Asia Pte Ltd, 2016-2021 Servant Contributors
module Servant.ACL.Internal.Links where

import Data.Proxy
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import GHC.TypeLits (KnownSymbol, symbolVal)
import Servant.ACL.Internal.Classes
import Servant.API
import Servant.API.Generic

{-
linkIfAuthorized ::
  routes (AsACLT m) ->
  (routes (AsACLT m) -> check) ->
  m (MkLink endpoint Link)
linkIfAuthorized routes getter = do
  getter routes
  pure $ fieldLink getter

-- | Like `linkIfAllowed`, but instead of throwing in the monad @m@ when the
-- action is not authorized, returns a @Nothing@.
linkMaybeIfAuthorized ::
  (routes (AsACLT m) -> endpoint) -> m (Maybe (MkLink endpoint Link))
linkMaybeIfAuthorized getter = _
-}

-- * HasAuthorizedLink

-- | Like HasLink, but uses the same arguments as HasACL.

-- It's quite inelegant that we basically copy the HasLink code, adding a few
-- consts.

class HasAuthorizedLink endpoint where
  type MkAuthorizedLink endpoint a
  toAuthorizedLink ::
    (Link -> a) ->
    Proxy endpoint ->
    Link ->
    MkAuthorizedLink endpoint a

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
  type MkAuthorizedLink (Vault :> sub) a = MkAuthorizedLink sub a
  toAuthorizedLink = simpleToLink (Proxy :: Proxy sub)

instance HasAuthorizedLink sub => HasAuthorizedLink (Description s :> sub) where
  type MkAuthorizedLink (Description s :> sub) a = MkAuthorizedLink sub a
  toAuthorizedLink = simpleToLink (Proxy :: Proxy sub)

instance HasAuthorizedLink sub => HasAuthorizedLink (Summary s :> sub) where
  type MkAuthorizedLink (Summary s :> sub) a = MkAuthorizedLink sub a
  toAuthorizedLink = simpleToLink (Proxy :: Proxy sub)

instance HasAuthorizedLink sub => HasAuthorizedLink (HttpVersion :> sub) where
  type MkAuthorizedLink (HttpVersion :> sub) a = MkAuthorizedLink sub a
  toAuthorizedLink = simpleToLink (Proxy :: Proxy sub)

instance HasAuthorizedLink sub => HasAuthorizedLink (IsSecure :> sub) where
  type MkAuthorizedLink (IsSecure :> sub) a = MkAuthorizedLink sub a
  toAuthorizedLink = simpleToLink (Proxy :: Proxy sub)

instance HasAuthorizedLink sub => HasAuthorizedLink (WithNamedContext name context sub) where
  type MkAuthorizedLink (WithNamedContext name context sub) a = MkAuthorizedLink sub a
  toAuthorizedLink toA _ = toAuthorizedLink toA (Proxy :: Proxy sub)

instance HasAuthorizedLink sub => HasAuthorizedLink (RemoteHost :> sub) where
  type MkAuthorizedLink (RemoteHost :> sub) a = MkAuthorizedLink sub a
  toAuthorizedLink = simpleToLink (Proxy :: Proxy sub)

instance HasAuthorizedLink sub => HasAuthorizedLink (BasicAuth realm a :> sub) where
  type MkAuthorizedLink (BasicAuth realm a :> sub) r = MkAuthorizedLink sub r
  toAuthorizedLink = simpleToLink (Proxy :: Proxy sub)

instance HasAuthorizedLink EmptyAPI where
  type MkAuthorizedLink EmptyAPI a = EmptyAPI
  toAuthorizedLink _ _ _ = EmptyAPI

--

-- Verb (terminal) instances
instance HasAuthorizedLink (Verb m s ct a) where
  type MkAuthorizedLink (Verb m s ct a) r = r
  toAuthorizedLink toA _ = toA

instance HasAuthorizedLink Raw where
  type MkAuthorizedLink Raw a = a
  toAuthorizedLink toA _ = toA

instance HasAuthorizedLink (Stream m fr ct a) where
  type MkAuthorizedLink (Stream m fr ct a) r = r
  toAuthorizedLink toA _ = toA
