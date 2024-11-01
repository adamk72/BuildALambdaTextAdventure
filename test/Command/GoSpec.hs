module Command.GoSpec (spec) where

import           Command.Go
import           Control.Monad.State
import           Core.State
import           Data.List            (find)
import           Data.Text            (Text)
import           Mock.GameEnvironment
import           Test.Hspec

-- Helper function to execute state and get both result and final state
runGoCommand :: Maybe Text -> GameWorld -> (Text, GameWorld)
runGoCommand cmd = runState (executeGo cmd)

spec :: Spec
spec = describe "executeGo" $ do
    let startLocTag = "meadow"
        allowFromStartTag = "cave"
        noAllowFromStartTag = "forest"

    context "check testing assumptions" $ do
        it "should start the active character in the meadow" $ do
            let acLoc = locTag $ currentLocation $ activeCharacter testGW
            acLoc `shouldBe` startLocTag
        it "should have all location tags in list of locations" $ do
            let locs = locations testGW
            (find (\x -> locTag x == startLocTag) locs) `shouldBe` (Just testMeadow)
            (find (\x -> locTag x == allowFromStartTag) locs) `shouldBe` (Just testCave)
            (find (\x -> locTag x == noAllowFromStartTag) locs) `shouldBe` (Just testForest)

    context "when given a valid location" $ do
        it "prevents going to an unconnected location" $ do
            let (result, newState) = runGoCommand (Just noAllowFromStartTag) testGW
            result `shouldBe` renderMessage (UnknownLocation noAllowFromStartTag)
            newState `shouldBe` testGW

        it "allows moving to a new, connected location" $ do
            let (result, newState) = runGoCommand (Just allowFromStartTag) testGW
            result `shouldBe` renderMessage (MovingToLocation allowFromStartTag)
            currentLocation (activeCharacter newState) `shouldBe` testCave

        it "prevents moving to current location" $ do
            let (result, newState) = runGoCommand (Just startLocTag) testGW
            result `shouldBe` renderMessage (AlreadyAtLocation startLocTag)
            newState `shouldBe` testGW

    context "when given an invalid location" $ do
        it "handles unknown locations" $ do
            let (result, newState) = runGoCommand (Just "nonexistent") testGW
            result `shouldBe` renderMessage (UnknownLocation "nonexistent")
            newState `shouldBe` testGW

    context "when given no location" $ do
        it "handles Nothing input" $ do
            let (result, newState) = runGoCommand Nothing testGW
            result `shouldBe` renderMessage NoLocationSpecified
            newState `shouldBe` testGW
