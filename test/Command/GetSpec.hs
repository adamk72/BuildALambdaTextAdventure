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
    let ac = activeCharacter defaultGW
        startLoc = getActiveEntityLocFromGW activeCharacter defaultGW
    context "check testing assumptions" $ do
        it "should have the silver coin in the starting location" $ do
            let coin = List.find (\inter -> getTag inter == "silver coin") (interactables defaultGW)
            fmap getLocation coin `shouldBe` Just startLoc
        it "should have the active character in the correct starting location" $ do
            getLocation ac `shouldBe` startLoc

    context "when picking up objects" $ do
        let (_, newState) = runGetCommand (Just "silver coin") defaultGW
            coin = List.find (\inter -> getTag inter == "silver coin") (interactables newState)
            expectedLoc = fromJust $ findLocationInInventory "alice" ac
            objs = Prelude.filter (\inter -> getLocation inter == getLocation ac) (interactables newState)
        it "can transfer sliver coin from location to person" $ do
            fmap getLocation coin `shouldBe` Just expectedLoc
        it "is no longer an element in the environment" $ do
            notElem (fromJust coin) objs `shouldBe` True


        -- it "handles attempting to get nonexistent objects" $ do
        --     let (result, newState) = runGetCommand (Just "nonexistent") defaultGW
        --     result `shouldBe` renderMessage (ObjectNotFound "nonexistent")
        --     newState `shouldBe` defaultGW

    -- context "when given no object" $
    --     it "handles Nothing input" $ do
    --         let (result, newState) = runGetCommand Nothing defaultGW
    --         result `shouldBe` renderMessage NoObjectSpecified
    --         newState `shouldBe` defaultGW
