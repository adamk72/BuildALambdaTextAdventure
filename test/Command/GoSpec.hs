module Command.GoSpec (spec) where

import           Command.Go
import           Control.Monad.State
import           Core.State
import           Data.Text           (Text)
import           Test.Hspec

-- Helper function to create a default GameWorld for testing
defaultGameWorld :: GameWorld
defaultGameWorld = GameWorld
    { activeCharacter = Character
        {
          charTag = "alice",
          charName = "Alice the Adventurer",
          currentLocation = Location
            { locTag = "cave"
            , locName = "A dark cave"
            }
        },
      playableCharacters = [],
      locations = [Location
            { locTag = "cave"
            , locName = "A dark cave"
            }, Location
            { locTag = "meadow"
            , locName = "A flowery meadow"
            }]
    }

-- Helper function to execute state and get both result and final state
runGoCommand :: Maybe Text -> GameWorld -> (Text, GameWorld)
runGoCommand cmd = runState (executeGo cmd)

spec :: Spec
spec = describe "executeGo" $ do
    context "when given a valid location" $ do
        it "allows moving to a new location" $ do
            let (result, newState) = runGoCommand (Just "meadow") defaultGameWorld
            result `shouldBe` "Moving to meadow."
            (locName. currentLocation . activeCharacter) newState `shouldBe` "A flowery meadow"
            (locTag . currentLocation . activeCharacter) newState `shouldBe` "meadow"

        it "prevents moving to current location" $ do
            let (result, newState) = runGoCommand (Just "cave") defaultGameWorld
            result `shouldBe` "You're already in cave."
            newState `shouldBe` defaultGameWorld

    context "when given an invalid location" $ do
        it "handles unknown locations" $ do
            let (result, newState) = runGoCommand (Just "nonexistent") defaultGameWorld
            result `shouldBe` "Unknown location: nonexistent."
            newState `shouldBe` defaultGameWorld

    context "when given no location" $ do
        it "handles Nothing input" $ do
            let (result, newState) = runGoCommand Nothing defaultGameWorld
            result `shouldBe` "Unable to find a location at all."
            newState `shouldBe` defaultGameWorld
