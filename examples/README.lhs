# servant-acl

`servant-acl` is a package for dealing with access-control (ACL) in `servant`
applications.

~~~ haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.ACL
import Servant.API.Generic
import Servant.Server.Generic

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

dogsServer :: DogsAPI AsServer
dogsServer = DogsAPI
  { getDog = getDogHandler
  , newPuppy = newPuppyHandler
  }

dogsACL :: DogsAPI AsACL
dogsACL = DogsAPI
  { getDog = getDogACL
  , newPuppy = newPuppyACL
  }

getDogHandler :: String -> Handler Dog
getDogHandler = _

getDogACL :: String -> Handler ()
getDogACL = _

newPuppyHandler :: String -> Handler Dog
newPuppyHandler = _

newPuppyACL :: String -> Handler ()
newPuppyACL = _


main :: IO ()
main = run 8080 $ serveWithACL (Proxy :: Proxy DogsAPI) dogsACL dogsServer
~~~
