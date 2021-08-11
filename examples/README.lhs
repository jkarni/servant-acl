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
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
import Data.Aeson (ToJSON, FromJSON)
import Control.Monad.Reader
import Control.Monad.Error.Class
import Data.Foldable (find)
import Data.IORef
import Data.Text.Encoding (decodeUtf8)
import Data.Text (unpack)
import Network.Wai.Handler.Warp (run)
import Servant hiding (Link)
import Servant.ACL
import Servant.API.Generic
import Servant.Server.Generic

data WholeAPI a = WholeAPI
  { dogsAPI :: a :- BasicAuth "dogs" User :> ToServantApi DogsAPI
  }
  deriving (Generic)

server :: WholeAPI (AsServerT (WithACL Kennel))
server = WholeAPI { dogsAPI = toServant . dogsServer }

-- We define the API as usual
data DogsAPI a = DogsAPI
  { getDog :: a :- Capture "name" String :> Get '[JSON] Dog
  , newPuppy :: a :- ReqBody '[JSON] String :> Post '[JSON] (Linked Dog)
  }
  deriving (Generic)

data Dog = Dog
  { name :: String
  , age :: Int
  }
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

dogsServer :: User -> DogsAPI (AsServerT (WithACL Kennel))
dogsServer user = DogsAPI
  { getDog = \name' -> getDogHandler user name' `withACL_` getDogACL user name'
  , newPuppy = \name' -> newPuppyHandler user name' `withACL_` newPuppyACL user name'
  }

-- We also do authentication.
data User = User
  { username :: String
  }


newtype Kennel a = Kennel { getKennel :: ReaderT (IORef [Dog]) Handler a }
  deriving newtype (Functor, Applicative, Monad, MonadError ServerError,
            MonadReader (IORef [Dog]), MonadIO)

-- | Here we have the handler as usual.
getDogHandler :: User -> String -> Kennel Dog
getDogHandler _user name' = do
  dogs <- liftIO . readIORef =<< ask
  case find (\x -> name x == name') dogs of
    Nothing -> throwError $ err404
    Just dog -> pure dog

-- | Here are the permission-related checks.
getDogACL :: User -> String -> Kennel ()
getDogACL user _name' = do
  when (username user == "jkarni") $
    throwError err401

-- | Again the handler as usual.
newPuppyHandler :: User -> String -> Kennel (Linked Dog)
newPuppyHandler user name' = do
  let dog = (Dog name' 0)
  ask >>= \r -> liftIO $ modifyIORef r (dog :)
  let dogLinks :: DogsAPI (AsAuthorizedLink (Kennel Link))
      dogLinks = fromServant $ dogsAPI links user
  dog `withLinks`
    [("self", getDog dogLinks name' )]

-- | And again its permissions check.
newPuppyACL :: User -> String -> Kennel ()
newPuppyACL _ _ = pure ()

links :: WholeAPI (AsAuthorizedLink (Kennel Link))
links = allAuthorizedFieldLinks server

main :: IO ()
main = do
  putStrLn "servant-acl example"
  ref <- newIORef []
  let nat hdl = runReaderT (getKennel $ runWithACL hdl) ref
      api = genericApi (Proxy :: Proxy WholeAPI)
      checkAuth = BasicAuthCheck $ \d ->
        if (decodeUtf8 (basicAuthPassword d) == "supersecret")
           then return (Authorized $ User $ unpack $ decodeUtf8 $ basicAuthUsername d)
           else return BadPassword
  run 8080
    $ serveWithContext api (checkAuth :. EmptyContext)
    $ hoistServerWithContext api (Proxy :: Proxy '[BasicAuthCheck User]) nat
    $ toServant server
~~~
