{-# LANGUAGE OverloadedStrings #-}
module Command.PutSpec (spec) where

import           Command.Commands
import           Command.TestUtils
import           Command.Message
import           Core.State
import Mock.TestWorld
import           Parser.Types
import           Test.Hspec

spec :: Spec
spec = do
    describe "Put Command" $ do
        describe "expression handling" $ do
            it "handles atomic expression (just 'put')" $ do
                let gw = defaultGW
                    expr = AtomicCmdExpression "put"
                (output, newState) <- runCommand executePut expr gw

                output `shouldBe` renderMessage PutWhat
                verifyStartLocation newState "meadow"

            it "handles unary expression (put <item>)" $ do
                let gw = defaultGW
                    expr = UnaryCmdExpression "put" (NounClause "silver coin")
                (output, newState) <- runCommand executePut expr gw

                output `shouldBe` renderMessage (PutWhere "silver coin")
                verifyStartLocation newState "meadow"

            it "handles binary expression (put in <container>)" $ do
                let gw = defaultGW
                    expr = BinaryCmdExpression "put" (PrepClause "in") (NounClause "bag of holding")
                (output, newState) <- runCommand executePut expr gw

                output `shouldBe` renderMessage PutWhat
                verifyStartLocation newState "meadow"
