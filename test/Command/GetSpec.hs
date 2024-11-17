{-# LANGUAGE OverloadedStrings #-}
module Command.GetSpec (spec) where

import           Command.Commands
import           Command.TestUtils
import           Core.Message
import           Core.State
import           Data.Maybe
import Mock.TestWorld
import           Parser.Types
import           Test.Hspec

spec :: Spec
spec = do
    describe "Get Command" $ do
        describe "expression handling" $ do
            it "handles atomic expression (just 'get')" $ do
                let gw = defaultGW
                    expr = AtomicExpression "get"
                (output, newState) <- runCommand executeGet expr gw

                output `shouldBe` renderMessage GetWhat
                checkItemTagInPocket "silver coin" newState `shouldBe` False

            it "handles unary expression (get <item>)" $ do
                let gw = defaultGW
                    expr = UnaryExpression "get" (NounClause "silver coin")
                (output, newState) <- runCommand executeGet expr gw

                output `shouldBe` "Moved silver coin to Alice the Adventurer"
                checkItemTagInPocket "silver coin" newState `shouldBe` True

            it "handles binary expression (get <item> from <location>)" $ do
                let gw = defaultGW
                    expr = BinaryExpression "get" (PrepClause "from") (NounClause "someplace")
                (output, newState) <- runCommand executeGet expr gw

                output `shouldBe` renderMessage GetWhat
                checkItemTagInPocket "silver coin" newState `shouldBe` False

            it "handle complex sentences for picking something up" $ do
                let gw = defaultGW
                    expr = ComplexExpression "get" (NounClause "silver coin") (PrepClause "from") (NounClause "meadow")
                (output, newState) <- runCommand executeGet expr gw

                output `shouldBe` renderMessage (PickedUp "silver coin" "Alice the Adventurer")
                checkItemTagInPocket "silver coin" newState `shouldBe` True
