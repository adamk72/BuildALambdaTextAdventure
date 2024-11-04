module Command.GoSpec (spec) where

import           Command.Go
import           Control.Exception    (ErrorCall (..), evaluate)
import           Control.Monad.State
import           Core.State
import           Data.List            (find)
import           Data.Text            (Text, unpack)
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
            let acLoc = locTag $ getActiveCharLoc defaultGW
            acLoc `shouldBe` startLocTag

        it "should have all location tags in list of gwLocations" $ do
            let locs = gwLocations defaultGW
            find (\x -> locTag x == startLocTag) locs `shouldBe` Just testMeadow
            find (\x -> locTag x == allowFromStartTag) locs `shouldBe` Just testCave
            find (\x -> locTag x == noAllowFromStartTag) locs `shouldBe` Just testForest

    context "when given a valid location" $ do
        it "prevents going to an unconnected location" $ do
            let (result, newState) = runGoCommand (Just noAllowFromStartTag) defaultGW
            result `shouldBe` renderMessage (NoPath noAllowFromStartTag)
            newState `shouldBe` defaultGW

        it "allows moving to a new, connected location" $ do
            let (result, newState) = runGoCommand (Just allowFromStartTag) defaultGW
            result `shouldBe` renderMessage (MovingToLocation allowFromStartTag)
            getLocation (gwActiveCharacter newState) `shouldBe` testCave

        it "prevents moving to current location" $ do
            let (result, newState) = runGoCommand (Just startLocTag) defaultGW
            result `shouldBe` renderMessage (AlreadyAtLocation startLocTag)
            newState `shouldBe` defaultGW

    context "when given an invalid location" $ do
        it "handles unknown gwLocations" $ do
            let (result, newState) = runGoCommand (Just "nonexistent") defaultGW
            result `shouldBe` renderMessage (NoPath "nonexistent")
            newState `shouldBe` defaultGW

        it "errors out on bad starting location" $ do
            -- Character tries moving to 'cave' which is not in the gwLocations list, but there is a locTag to
            -- from the character's current location; there's a JSON mismatch that breaks the game.
            let badSetupWorld = withLocations (withCharacterAt defaultGW testForest) [testMeadow]
                caveResult = evaluate (runGoCommand (Just "cave") badSetupWorld)
            caveResult `shouldThrow` (\(ErrorCall msg) -> msg == unpack (renderMessage $ DoesNotExist "cave"))

            -- alternate versions of badSetupWorld:
            -- let badSetupWorld = (flip withLocations [testMeadow] . flip withCharacterAt testForest) defaultGW
            -- let badSetupWorld = defaultGW & flip withCharacterAt testForest & flip withLocations [testMeadow]

    context "when given no location" $ it "handles Nothing input" $ do
        let (result, newState) = runGoCommand Nothing defaultGW
        result `shouldBe` renderMessage NoLocationSpecified
        newState `shouldBe` defaultGW
