{-# LANGUAGE QuasiQuotes #-}

module Servant.ACLSpec
  ( spec,
  )
where

import Data.Aeson
import Data.Aeson.QQ (aesonQQ)
import GHC.Generics (Generic)
import Servant.ACL
import Test.Hspec

spec :: Spec
spec = do
  linkedSpec

linkedSpec :: Spec
linkedSpec = describe "Linked" $ do
  it "encodes links as a flat object" $ do
    let x = Linked {value = Foo 5, _links = mempty}
    toJSON x `shouldBe` [aesonQQ| { "bar": 5, "_links": [] } |]

data Foo = Foo {bar :: Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)
