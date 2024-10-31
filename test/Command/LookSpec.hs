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
                let result = evalState (executeLook (Just dir)) defaultGameWorld
                in T.isPrefixOf "You look " result
                   && T.isSuffixOf "but see nothing special." result

        -- Property: All valid directions should be recognized by isDirectionalLook
        it "recognizes all valid directions" $ property $
            \(ValidDirection dir) ->
                isDirectionalLook dir === Just dir

        -- Property: Invalid directions should return Nothing from isDirectionalLook
        it "rejects invalid directions" $ property $
            \(InvalidDirection dir) ->
                isDirectionalLook dir === Nothing

    describe "executeLook" $ do
        context "when looking with no direction (default look)" $ do
            it "returns basic location description" $ do
                let (result, _) = runLookCommand Nothing defaultGameWorld
                result `shouldBe` "You are in A dark cave."

        context "when looking 'around'" $ do
            it "returns detailed location description" $ do
                let (result, _) = runLookCommand (Just "around") defaultGameWorld
                result `shouldBe` "You are in A dark cave. You look around carefully, taking in your surroundings."

        context "when looking in valid directions" $ do
            it "handles looking north" $ do
                let (result, _) = runLookCommand (Just "north") defaultGameWorld
                result `shouldBe` "You look north, but see nothing special."

            it "handles looking south" $ do
                let (result, _) = runLookCommand (Just "south") defaultGameWorld
                result `shouldBe` "You look south, but see nothing special."

            it "handles looking east" $ do
                let (result, _) = runLookCommand (Just "east") defaultGameWorld
                result `shouldBe` "You look east, but see nothing special."

            it "handles looking west" $ do
                let (result, _) = runLookCommand (Just "west") defaultGameWorld
                result `shouldBe` "You look west, but see nothing special."

        context "when looking in invalid directions" $ do
            it "handles invalid direction gracefully" $ do
                let (result, _) = runLookCommand (Just "up") defaultGameWorld
                result `shouldBe` "You look up, but see nothing special."

        context "state remains unchanged after looking" $ do
            it "doesn't modify game state when looking" $ do
                let (_, newState) = runLookCommand Nothing defaultGameWorld
                newState `shouldBe` defaultGameWorld

    describe "isDirectionalLook" $ do
        context "with valid directions" $ do
            it "recognizes 'north' as a valid direction" $ do
                isDirectionalLook "north" `shouldBe` Just "north"

            it "recognizes 'south' as a valid direction" $ do
                isDirectionalLook "south" `shouldBe` Just "south"

            it "recognizes 'east' as a valid direction" $ do
                isDirectionalLook "east" `shouldBe` Just "east"

            it "recognizes 'west' as a valid direction" $ do
                isDirectionalLook "west" `shouldBe` Just "west"

        context "with invalid directions" $ do
            it "returns Nothing for 'up'" $ do
                isDirectionalLook "up" `shouldBe` Nothing

            it "returns Nothing for 'down'" $ do
                isDirectionalLook "down" `shouldBe` Nothing

            it "returns Nothing for empty string" $ do
                isDirectionalLook "" `shouldBe` Nothing

            it "returns Nothing for arbitrary text" $ do
                isDirectionalLook "random" `shouldBe` Nothing
