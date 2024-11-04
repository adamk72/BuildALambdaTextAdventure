module Command.DropSpec (spec) where

import           Command.Drop
import           Command.Utils
import           Data.Text
import           Mock.GameEnvironment
import           Test.Hspec

silver :: Text
silver = "silver coin"

spec :: Spec
spec = describe "" $ do
  context "Foo" $ do
    let cmdResult = runCommand executeDrop (Just silver ) defaultGW

    it "bar" $ do
      isPrefixOf "Items at locationz" (fst cmdResult) `shouldBe` True
