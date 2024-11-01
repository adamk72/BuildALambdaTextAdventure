module Command.GoSpec (spec) where

import           Command.Go
import           Control.Monad.State
import           Core.State
import           Data.Text           (Text)
import           Mock.GameEnvironment
import           Test.Hspec


-- Helper function to execute state and get both result and final state
runGoCommand :: Maybe Text -> GameWorld -> (Text, GameWorld)
runGoCommand cmd = runState (executeGo cmd)

spec :: Spec
spec = describe "executeGo" $ do
    context "when given a valid location" $ do
        it "allows moving to a new location" $ do
            let (result, newState) = runGoCommand (Just "meadow") defaultGameWorld
            result `shouldBe` renderMessage (MovingToLocation "meadow")
            currentLocation (activeCharacter newState) `shouldBe` testMeadow

        it "prevents moving to current location" $ do
            let (result, newState) = runGoCommand (Just "cave") defaultGameWorld
            result `shouldBe` renderMessage (AlreadyAtLocation "cave")
            newState `shouldBe` defaultGameWorld

    context "when given an invalid location" $ do
        it "handles unknown locations" $ do
            let (result, newState) = runGoCommand (Just "nonexistent") defaultGameWorld
            result `shouldBe` renderMessage (UnknownLocation "nonexistent")
            newState `shouldBe` defaultGameWorld

    context "when given no location" $ do
        it "handles Nothing input" $ do
            let (result, newState) = runGoCommand Nothing defaultGameWorld
            result `shouldBe` renderMessage NoLocationSpecified
            newState `shouldBe` defaultGameWorld