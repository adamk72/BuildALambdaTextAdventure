module Command.DropSpec (spec) where

import           Command.Drop
import           Command.Get
import           Command.Utils
import           Core.State.GameState
import           Core.State.TaggedEntity
import           Core.State.Operations
import           Data.Text
import           Mock.GameEnvironment
import Data.Maybe (fromJust)
import           Test.Hspec

silver :: Text
silver = "silver coin"

spec :: Spec
spec = describe "Execute drop" $ do
    let (_, getGW)= runCommand executeGet (Just silver) defaultGW
    context "Pre inventory check" $ do
      it "currently has the silver coin before dropping" $ do
        let itemLoc = fromJust (findItemByTag silver getGW)
        getLocation itemLoc `shouldBe` getPocketSlot getGW

    context "Post inventory check" $ do
      it "no longer possess a silver coin" $ do
        let (_, dropGW) = runCommand executeDrop (Just silver) getGW
            acAfter = gwActiveActor dropGW
            itemLoc = fromJust (findItemByTag silver dropGW)
        checkItemTagInPocket silver acAfter `shouldBe` False
        getLocation itemLoc `shouldBe` testMeadow
