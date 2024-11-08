{-# LANGUAGE OverloadedStrings #-}
module Command.GoSpec (spec) where

import           Command.Commands
import           Command.TestUtils
import           Core.State
import           Mock.GameEnvironment
import           Test.Hspec
import           Parser.Types

spec :: Spec
spec = do
    describe "Go Command" $ do
        describe "basic movement" $ do
            it "allows movement to valid locations" $ do
                let gw = defaultGW
                    cmd = "go cave"
                    (output, newState) = runCommand executeGo (UnaryExpression "go" (NounClause "cave")) gw
                
                output `shouldBe` "Moving to cave."
                verifyStartLocation newState "cave"

            it "prevents movement to invalid locations" $ do
                let gw = defaultGW
                    cmd = "go nowhere"
                    (output, newState) = runCommand executeGo (UnaryExpression "go" (NounClause "nowhere")) gw
                
                output `shouldBe` "Pending"  -- Note: This should be updated when error handling is completed
                verifyStartLocation newState "meadow"  -- Should remain in original location

        describe "destination validation" $ do
            it "validates destinations against location's destination tags" $ do
                let gw = defaultGW `withActorAt` testForest
                    cmd = "go meadow"
                    (output, _) = runCommand executeGo (UnaryExpression "go" (NounClause "meadow")) gw
                
                output `shouldBe` "Pending"  -- Should be updated when proper validation is added
