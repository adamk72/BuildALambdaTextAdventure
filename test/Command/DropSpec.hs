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
    let getResult = runCommand executeGet (Just silver) defaultGW
        acBefore = gwActiveActor (snd getResult)
    context "Pre inventory check" $ do
      it "currently has the silver coin before dropping" $ do
        let itemLoc = fromJust (findItemByTag silver (snd getResult))
        getLocation itemLoc `shouldBe` fromJust (findLocInInventoryByTag (getTag acBefore) acBefore)

    context "Post inventory check" $ do
      it "no longer possess a silver coin" $ do
        let dropResult = runCommand executeDrop (Just silver) (snd getResult)
            acAfter = gwActiveActor (snd dropResult)
            itemLoc = fromJust (findItemByTag silver (snd dropResult))
        (findLocInInventoryByTag silver acAfter) `shouldBe` Nothing
        getLocation itemLoc `shouldBe` testMeadow
