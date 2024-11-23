{-# LANGUAGE OverloadedStrings #-}
module Command.GoSpec (spec) where

import           Command.Commands
import           Command.TestUtils
import           Control.Exception    (evaluate)
import           Command.Message
import           Core.State
import           Data.Text            (unpack)
import           Mock.TestWorld
import           Parser.Types
import           Test.Hspec

spec :: Spec
spec = do
    describe "Go Command" $ do
        describe "expression handling" $ do
            it "handles atomic expression (just 'go')" $ do
                let gw = defaultGW
                    expr = AtomicExpression "go"
                (output, newState) <- runCommand executeGo expr gw

                output `shouldBe` renderMessage GoWhere -- Should be updated with proper error message
                verifyStartLocation newState "meadow"  -- Should not move

            it "handles unary expression (go <location>)" $ do
                let gw = defaultGW
                    expr = UnaryExpression "go" (NounClause "cave")
                (output, newState) <- runCommand executeGo expr gw

                output `shouldBe` "Moving to cave."
                verifyStartLocation newState "cave"

            it "handles binary expression (go to <location>)" $ do
                let gw = defaultGW
                    expr = BinaryExpression "go" (PrepClause "to") (NounClause "cave")
                (output, newState) <- runCommand executeGo expr gw

                output `shouldBe` "Moving to cave."
                verifyStartLocation newState "cave"

            it "handles complex expression as unknown" $ do
                let gw = defaultGW
                    expr = ComplexExpression "go" (NounClause "something") (PrepClause "to") (NounClause "cave")
                (output, newState) <- runCommand executeGo expr gw

                output `shouldBe` renderMessage NotSure
                verifyStartLocation newState "meadow"
