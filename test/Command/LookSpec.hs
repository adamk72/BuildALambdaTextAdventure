{-# LANGUAGE OverloadedStrings #-}
module Command.LookSpec (spec) where

import           Command.Look
import           Control.Monad.State
import           Core.State
import           Data.Text
import           Mock.GameEnvironment
import           Test.Hspec

-- Helper function to execute state and get both result and final state
runCommand :: Text -> GameWorld -> (Text, GameWorld)
runCommand cmd = runState (executeLook cmd)

spec :: Spec
spec = do
    describe "executeLook" $ do
        let startLocTag = "meadow"
            acLoc = getActiveActorLoc defaultGW
            acLocTag = locTag acLoc
            acLotName = locName acLoc
        context "check testing assumptions" $ do
            it "should start the active character in the meadow" $ do
                acLocTag `shouldBe` startLocTag

        context "when looking 'around'" $ do
            it "returns detailed location description" $ do
                let (result, _) = runCommand "around" defaultGW
                    objs = Prelude.filter (\item -> getLocation item == acLoc) (gwItems defaultGW)
                result `shouldBe` renderMessage (YouAreIn acLotName) <> " " <> renderMessage (LookAround objs)

