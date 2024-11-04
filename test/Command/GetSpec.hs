module Command.GetSpec (spec) where

-- import           Control.Exception    (ErrorCall (..), evaluate)
import           Command.Get
import           Control.Monad.State
import           Core.State
import           Data.List            as List (find)
import           Data.Maybe           (fromJust)
import           Data.Text            (Text)
import           Mock.GameEnvironment
import           Test.Hspec

runGetCommand :: Maybe Text -> GameWorld -> (Text, GameWorld)
runGetCommand cmd = runState (executeGet cmd)

spec :: Spec
spec = describe "executeGet" $ do
    context "check testing assumptions" $ do
        it "should have the silver coin in the starting location" $ do
            let startLoc = getActiveEntityLocFromGW activeCharacter defaultGW
                coin = List.find (\inter -> getTag inter == "silver coin") (interactables defaultGW)
            fmap getLocation coin `shouldBe` Just startLoc

        it "should have the active character in the correct starting location" $ do
            let ac = activeCharacter defaultGW
            getLocation ac `shouldBe` getActiveEntityLocFromGW activeCharacter defaultGW

    context "when picking up objects" $ do
        it "can transfer from location to person" $ do
            let (_, newState) = runGetCommand (Just "silver coin") defaultGW
                ac = activeCharacter defaultGW
                coin = List.find (\inter -> getTag inter == "silver coin") (interactables newState)
                expectedLoc = fromJust $ findLocationInInventory "alice" ac
            fmap getLocation coin `shouldBe` Just expectedLoc

        -- it "handles attempting to get nonexistent objects" $ do
        --     let (result, newState) = runGetCommand (Just "nonexistent") defaultGW
        --     result `shouldBe` renderMessage (ObjectNotFound "nonexistent")
        --     newState `shouldBe` defaultGW

    -- context "when given no object" $
    --     it "handles Nothing input" $ do
    --         let (result, newState) = runGetCommand Nothing defaultGW
    --         result `shouldBe` renderMessage NoObjectSpecified
    --         newState `shouldBe` defaultGW
