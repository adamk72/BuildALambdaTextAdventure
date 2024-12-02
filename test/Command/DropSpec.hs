{-# LANGUAGE OverloadedStrings #-}
module Command.DropSpec (spec) where

import           Command.Commands
import           Command.TestUtils
import           Command.Message
import Mock.TestWorld
import           Parser.Types
import           Test.Hspec

spec :: Spec
spec = do
    describe "Drop Command" $ do
        describe "expression handling" $ do
            it "handles atomic expression (just 'drop')" $ do
                let gw = defaultGW
                    expr = AtomicCmdExpression "drop"
                (output, newState) <- runCommand executeDrop expr gw

                output `shouldBe` renderMessage DropWhat
                verifyStartLocation newState "meadow"

            it "handles unary expression (drop <item>)" $ do
                -- First get the item
                let gw = defaultGW
                    getExpr = UnaryCmdExpression "get" (NounClause "silver coin")
                (_, midState) <- runCommand executeGet getExpr gw

                    -- Then drop it
                let dropExpr = UnaryCmdExpression "drop" (NounClause "silver coin")
                (output, finalState) <- runCommand executeDrop dropExpr midState

                output `shouldBe` renderMessage (DroppedItemWithInventory "silver coin" "")

            it "questions what is meant by an incomplete phrase" $ do
                let gw = defaultGW
                    expr = BinaryCmdExpression "drop" (PrepClause "on") (NounClause "ground")
                (output, _) <- runCommand executeDrop expr gw
                output `shouldBe` renderMessage DropWhat

            it "handles full phrase" $ do
                let gw = defaultGW
                    expr = ComplexCmdExpression "drop" (NounClause "silver coin") (PrepClause "on") (NounClause "ground")
                (output, newState) <- runCommand executeDrop expr gw

                output `shouldBe` renderMessage (DroppedItemSomewhere "silver coin" "on ground")

            it "handles dropping of all objects" $ do
                let gw = defaultGW
                    expr = ComplexCmdExpression "drop" (NounClause "all") (PrepClause "on") (NounClause "ground")
                (output, newState) <- runCommand executeDrop expr gw

                output `shouldBe` "IN DEVELOPMENT: dropping all items from inventory"
                verifyStartLocation newState "meadow"
