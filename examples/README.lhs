# servant-acl

(very WIP)

`servant-acl` is a package for dealing with access-control (ACL) in `servant`
applications. You can think of it as doing for authorization what
`servant-auth` does for authentication (though you're by no means need to
combined them).

The basic idea is having an "ACL handler" for each regular handler.
`servant-acl` then runs these checks before their corresponding handlers.
Separating out the ACL makes the code cleaner and more obvious. But it also has
further benefits, such as allowing checks to see if an action *would* be
allowed. This means you can easily make one check be the combination of two
others, for example. It also means HATEOAS becomes easier (see below).

It is designed to be used with the generic approach to servant.

## HATEOAS

TODO

## Avoiding recomputations

TODO

# Annotated example

Below is a fairly extensive annotated example of usage.

~~~ haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
import Data.Aeson (ToJSON, FromJSON)
import Control.Monad.Reader
import Control.Monad.Error.Class
import Data.Foldable (find)
import Data.IORef
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant hiding (BasicAuth)
import Servant.ACL
import Servant.API.Generic
import Servant.Auth.Server
import Servant.Server.Generic

data WholeAPI a = WholeAPI
  { dogsAPI :: a :- Auth '[BasicAuth] User :> ToServantApi DogsAPI
  }

server :: WholeAPI (AsServerT Kennel)
server = WholeAPI { dogsAPI = toServant dogsServer }

acl :: WholeAPI (AsACLT Kennel)
acl = WholeAPI { dogsAPI = toServant dogsACL }

-- We define the API as usual
data DogsAPI a = DogsAPI
  { getDog :: a :- Capture "name" String :> Get '[JSON] Dog
  , newPuppy :: a :- ReqBody '[JSON] String :> Post '[JSON] Dog
  }
  deriving (Generic)

data Dog = Dog
  { name :: String
  , age :: Int
  }
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

dogsServer :: User -> DogsAPI (AsServerT Kennel)
dogsServer user = DogsAPI
  { getDog = getDogHandler user
  , newPuppy = newPuppyHandler user
  }

-- We also do authentication. Here we use servant-auth, but anything else would
-- also work
data User = User
  { username :: String
  }

-- This section is new. Its structure mimics that of the server we're
-- augmenting with ACL
dogsACL :: User -> DogsAPI (AsACLT Kennel)
dogsACL user = DogsAPI
  { getDog = getDogACL user
  , newPuppy = newPuppyACL user
  }

newtype Kennel a = Kennel { getKennel :: ReaderT (IORef [Dog]) Handler a }
  deriving (Functor, Applicative, Monad, MonadError ServerError,
            MonadReader (IORef [Dog]), MonadIO)

-- | Here we have the handler as usual.
getDogHandler :: User -> String -> Kennel Dog
getDogHandler _user name' = do
  dogs <- liftIO . readIORef =<< ask
  case find (\x -> name x == name') dogs of
    Nothing -> throwError $ err404
    Just dog -> pure $
        dog

-- | Here are the permission-related checks.
getDogACL :: User -> String -> Kennel ()
getDogACL user name' = do
  when (username user == "jkarni") $
    throwError err401

-- | Again the handler as usual.
newPuppyHandler :: User -> String -> Kennel Dog
newPuppyHandler user name' = do
  dogs <- liftIO . readIORef =<< ask
  links <- linkIfAuthorized acl (\x -> newPuppy (_ $ dogsAPI x) name')
  case find (\x -> name x == name') dogs of
    Nothing -> throwError $ err404
    Just dog ->
        pure dog

-- | And again its permissions check.
newPuppyACL :: User -> String -> Kennel ()
newPuppyACL _ _ = pure ()


main :: IO ()
main = do
  ref <- newIORef []
  let nat hdl = runReaderT (getKennel hdl) ref
  run 8080 $ serveTWithACL (Proxy :: Proxy WholeAPI) nat acl server
~~~
