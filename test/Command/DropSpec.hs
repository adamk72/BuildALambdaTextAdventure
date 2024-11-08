module Command.DropSpec (spec) where

import           Command.Drop
import           Command.Get
import           Command.TestUtils
import           Core.State.Operations
import           Core.State.TaggedEntity
import           Data.Maybe              (fromJust)
import           Data.Text
import           Mock.GameEnvironment
import           Test.Hspec

silver :: Text
silver = "silver coin"

spec :: Spec
spec = describe "Execute drop" $ do
    let (_, getGW)= runCommand executeGet silver defaultGW
    context "Pre inventory check" $ do
      it "currently has the silver coin before dropping" $ do
        let itemLoc = fromJust (findItemByTag silver getGW)
        getLocation itemLoc `shouldBe` getActorInventory getGW

    context "Post inventory check" $ do
      it "no longer possess a silver coin" $ do
        let (_, dropGW) = runCommand executeDrop silver getGW
            itemLoc = fromJust (findItemByTag silver dropGW)
        checkItemTagInPocket silver dropGW `shouldBe` False
        getLocation itemLoc `shouldBe` testMeadow
