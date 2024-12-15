{-# LANGUAGE OverloadedStrings #-}

module Command.GoSpec (spec) where

import           Command.Commands
import           Command.Executor
import           Command.Message
import           Command.TestUtils
import           Mock.TestWorld
import           Parser.Types
import           Test.Hspec

spec :: Spec
spec = do
  describe "Go Command" $ do
    describe "expression handling" $ do
      it "handles atomic expression (just 'go')" $ do
        let gw = defaultGW
            expr = AtomicCmdExpression "go"
        (output, newState) <- runCommand (runScenarioCheck executeGo) expr gw

        output `shouldBe` renderMessage GoWhere
        verifyStartLocation newState "meadow"

      it "handles unary expression (go <location>)" $ do
        let gw = defaultGW
            expr = UnaryCmdExpression "go" (NounClause "cave")
        (output, newState) <- runCommand (runScenarioCheck executeGo) expr gw

        output `shouldBe` "Moving to cave."
        verifyStartLocation newState "cave"

      it "handles binary expression (go to <location>)" $ do
        let gw = defaultGW
            expr = SplitCmdExpression "go" (PrepClause "to") (NounClause "cave")
        (output, newState) <- runCommand (runScenarioCheck executeGo) expr gw

        output `shouldBe` "Moving to cave."
        verifyStartLocation newState "cave"

      it "handles complex expression as unknown" $ do
        let gw = defaultGW
            expr = ComplexCmdExpression "go" (NounClause "something") (PrepClause "to") (NounClause "cave")
        (output, newState) <- runCommand (runScenarioCheck executeGo) expr gw

        output `shouldBe` renderMessage NotSure
        verifyStartLocation newState "meadow"
