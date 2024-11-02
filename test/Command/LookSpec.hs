{-# LANGUAGE OverloadedStrings #-}
module Command.LookSpec (spec) where

import           Command.Look
import           Control.Monad.State
import           Core.State
import qualified Data.Text            as T
import           Mock.GameEnvironment
import           Test.Hspec
import Test.QuickCheck

-- Helper function to execute state and get both result and final state
runLookCommand :: Maybe T.Text -> GameWorld -> (T.Text, GameWorld)
runLookCommand cmd = runState (executeLook cmd)

newtype ValidDirection = ValidDirection T.Text
    deriving (Show, Eq)

instance Arbitrary ValidDirection where
    arbitrary = ValidDirection . T.pack <$> elements ["north", "south", "east", "west"]

newtype InvalidDirection = InvalidDirection T.Text
    deriving (Show, Eq)

instance Arbitrary InvalidDirection where
    arbitrary = do
        -- Generate random text that isn't a valid direction
        txt <- arbitrary `suchThat` (`notElem` ["north", "south", "east", "west"])
        return $ InvalidDirection $ T.pack txt

spec :: Spec
spec = do
    describe "Look Command QuickCheck Properties" $ do
        -- Property: Looking in any valid direction should return a consistent format
        it "returns consistent format for all valid directions" $ property $
            \(ValidDirection dir) ->
                let result = evalState (executeLook (Just dir)) defaultGW
                in T.isPrefixOf "You look " result
                   && T.isSuffixOf "but see nothing special." result

    describe "executeLook" $ do
        let startLocTag = "meadow"
            acLocTag = locTag $ currentLocation $ activeCharacter defaultGW
            acLotName = locName $ currentLocation $ activeCharacter defaultGW
        context "check testing assumptions" $ do
            it "should start the active character in the meadow" $ do
                acLocTag `shouldBe` startLocTag

        context "when looking with no direction (default look)" $ do
            it "returns basic location description" $ do
                let (result, _) = runLookCommand Nothing defaultGW
                result `shouldBe` renderMessage (YouAreIn (locName testMeadow))

        context "when looking 'around'" $ do
            it "returns detailed location description" $ do
                let (result, _) = runLookCommand (Just "around") defaultGW
                result `shouldBe` renderMessage (YouAreIn acLotName) <> " " <> renderMessage LookAround


        context "when looking in invalid directions" $ do
            it "handles invalid direction gracefully" $ do
                let (result, _) = runLookCommand (Just "up") defaultGW
                result `shouldBe` "You look up, but see nothing special."

        context "state remains unchanged after looking" $ do
            it "doesn't modify game state when looking" $ do
                let (_, newState) = runLookCommand Nothing defaultGW
                newState `shouldBe` defaultGW