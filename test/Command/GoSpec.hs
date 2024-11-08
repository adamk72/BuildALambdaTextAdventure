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
        describe "expression handling" $ do
            it "handles atomic expression (just 'go')" $ do
                let gw = defaultGW
                    expr = AtomicExpression "go"
                    (output, newState) = runCommand executeGo expr gw

                output `shouldBe` "TO BE FIXED"  -- Should be updated with proper error message
                verifyStartLocation newState "meadow"  -- Should not move

            it "handles unary expression (go <location>)" $ do
                let gw = defaultGW
                    expr = UnaryExpression "go" (NounClause "cave")
                    (output, newState) = runCommand executeGo expr gw

                output `shouldBe` "Moving to cave."
                verifyStartLocation newState "cave"

            it "handles binary expression (go to <location>)" $ do
                let gw = defaultGW
                    expr = BinaryExpression "go" (PrepClause "to") (NounClause "cave")
                    (output, newState) = runCommand executeGo expr gw

                output `shouldBe` "Moving to cave."
                verifyStartLocation newState "cave"

            it "handles complex expression (should treat same as binary)" $ do
                let gw = defaultGW
                    expr = ComplexExpression "go" (NounClause "self") (PrepClause "to") (NounClause "cave")
                    (output, newState) = runCommand executeGo expr gw

                output `shouldBe` "TO BE FIXED"  -- Should be updated with proper error message
                verifyStartLocation newState "meadow"  -- Should not move

        describe "destination validation" $ do
            it "allows movement to valid adjacent locations" $ do
                let gw = defaultGW
                    expr = UnaryExpression "go" (NounClause "cave")
                    (output, newState) = runCommand executeGo expr gw

                output `shouldBe` "Moving to cave."
                verifyStartLocation newState "cave"

            it "prevents movement to non-adjacent locations" $ do
                let gw = defaultGW `withActorAt` testCave
                    expr = UnaryExpression "go" (NounClause "nowhere")
                    (output, newState) = runCommand executeGo expr gw

                output `shouldBe` "TO BE FIXED"  -- Should be updated when error handling is completed
                verifyStartLocation newState "cave"  -- Should remain in original location

            it "prevents movement to non-existent locations" $ do
                let gw = defaultGW
                    expr = UnaryExpression "go" (NounClause "narnia")
                    (output, newState) = runCommand executeGo expr gw

                output `shouldBe` "TO BE FIXED"  -- Should be updated with proper error message
                verifyStartLocation newState "meadow"  -- Should not move

            it "validates destinations against location's destination tags" $ do
                let gw = defaultGW `withActorAt` testForest
                    expr = UnaryExpression "go" (NounClause "meadow")  -- Forest only connects to cave
                    (output, newState) = runCommand executeGo expr gw

                output `shouldBe` "TO BE FIXED"  -- Should be updated when proper validation is added
                verifyStartLocation newState "forest"  -- Should remain in forest

        describe "state changes" $ do
            it "updates actor location after successful movement" $ do
                let gw = defaultGW
                    expr = UnaryExpression "go" (NounClause "cave")
                    (_, newState) = runCommand executeGo expr gw

                verifyStartLocation newState "cave"
                getLocation (gwActiveActor newState) `shouldBe` testCave

            it "maintains inventory when moving" $ do
                let gw = defaultGW  -- Assuming we have items in inventory
                    expr = UnaryExpression "go" (NounClause "cave")
                    (_, newState) = runCommand executeGo expr gw
                    originalInventory = getActorInventoryItems gw
                    newInventory = getActorInventoryItems newState

                newInventory `shouldMatchList` originalInventory