{-# LANGUAGE OverloadedStrings #-}
module Command.GoSpec (spec) where

import           Command.Commands
import           Command.TestUtils
import           Control.Exception    (evaluate)
import           Core.State
import           Data.Text            (unpack)
import           Mock.GameEnvironment
import           Parser.Types
import           Test.Hspec

spec :: Spec
spec = do
    describe "Go Command" $ do
        describe "expression handling" $ do
            it "handles atomic expression (just 'go')" $ do
                let gw = defaultGW
                    expr = AtomicExpression "go"
                    (output, newState) = runCommand executeGo expr gw

                output `shouldBe` renderMessage GoWhere -- Should be updated with proper error message
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

            it "handles complex expression as unknown" $ do
                let gw = defaultGW
                    expr = ComplexExpression "go" (NounClause "something") (PrepClause "to") (NounClause "cave")
                    (output, newState) = runCommand executeGo expr gw

                output `shouldBe` renderMessage NotSure
                verifyStartLocation newState "meadow"

        describe "destination validation" $ do
            it "allows movement to valid adjacent locations" $ do
                let gw = defaultGW
                    expr = UnaryExpression "go" (NounClause "cave")
                    (output, newState) = runCommand executeGo expr gw

                output `shouldBe` "Moving to cave."
                verifyStartLocation newState "cave"

            it "prevents movement to non-adjacent locations" $ do
                let gw = defaultGW `withActorAt` testMeadow
                    expr = UnaryExpression "go" (NounClause "forest")
                    (output, newState) = runCommand executeGo expr gw

                output `shouldBe` (renderMessage $ NoPath "forest") -- because forest is not next to meadow
                verifyStartLocation newState "meadow"

            it "prevents movement to non-existent locations" $ do
                let gw = defaultGW
                    expr = UnaryExpression "go" (NounClause "narnia")
                evaluate (runCommand executeGo expr gw) `shouldThrow`
                    (errorCall (unpack $ renderMessage $ LocationError "narnia"))

            it "validates destinations against location's destination tags" $ do
                let gw = defaultGW `withActorAt` testForest
                    expr = UnaryExpression "go" (NounClause "cave")
                    (output, newState) = runCommand executeGo expr gw

                output `shouldBe` (renderMessage $ MovingToLocation "cave") -- b/c cave is next to forest
                verifyStartLocation newState "cave"

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
