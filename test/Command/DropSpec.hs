{-# LANGUAGE OverloadedStrings #-}
module Command.DropSpec (spec) where

import           Command.Commands
import           Command.TestUtils
import           Core.Message
import           Core.State
import Mock.TestWorld
import           Parser.Types
import           Test.Hspec

spec :: Spec
spec = do
    describe "Drop Command" $ do
        describe "expression handling" $ do
            it "handles atomic expression (just 'drop')" $ do
                let gw = defaultGW
                    expr = AtomicExpression "drop"
                (output, newState) <- runCommand executeDrop expr gw

                output `shouldBe` renderMessage DropWhat
                verifyStartLocation newState "meadow"

            it "handles unary expression (drop <item>)" $ do
                -- First get the item
                let gw = defaultGW
                    getExpr = UnaryExpression "get" (NounClause "silver coin")
                (_, midState) <- runCommand executeGet getExpr gw

                    -- Then drop it
                let dropExpr = UnaryExpression "drop" (NounClause "silver coin")
                (output, finalState) <- runCommand executeDrop dropExpr midState

                output `shouldBe` renderMessage (DroppedItemWithInventory "silver coin" "") -- no inventory at this time
                checkItemTagInPocket "silver coin" finalState `shouldBe` False

            it "questions what is meant by an incomplete phrase" $ do
                let gw = defaultGW
                    expr = BinaryExpression "drop" (PrepClause "on") (NounClause "ground")
                (output, _) <- runCommand executeDrop expr gw
                output `shouldBe` renderMessage DropWhat

            it "handles full phrase" $ do
                let gw = defaultGW
                    expr = ComplexExpression "drop" (NounClause "silver coin") (PrepClause "on") (NounClause "ground")
                (output, newState) <- runCommand executeDrop expr gw

                output `shouldBe` renderMessage (DroppedItemSomewhere "silver coin" "on ground") -- no inventory at this time
                checkItemTagInPocket "silver coin" newState `shouldBe` False

            it "handles dropping of all objects" $ do
                let gw = defaultGW
                    expr = ComplexExpression "drop" (NounClause "all") (PrepClause "on") (NounClause "ground")
                (output, newState) <- runCommand executeDrop expr gw

                output `shouldBe` "IN DEVELOPMENT: dropping all items from inventory"
                verifyStartLocation newState "meadow"
