{-# LANGUAGE OverloadedStrings #-}
module Command.GetSpec (spec) where

import           Command.Commands
import           Command.TestUtils
import           Core.State
import           Mock.GameEnvironment
import           Test.Hspec
import           Parser.Types

spec :: Spec
spec = do
    describe "Get Command" $ do
        describe "expression handling" $ do
            it "handles atomic expression (just 'get')" $ do
                let gw = defaultGW
                    expr = AtomicExpression "get"
                    (output, newState) = runCommand executeGet expr gw

                output `shouldBe` "TO BE FIXED: get requires an item to pick up"
                checkItemTagInPocket "silver coin" newState `shouldBe` False

            it "handles unary expression (get <item>)" $ do
                let gw = defaultGW
                    expr = UnaryExpression "get" (NounClause "silver coin")
                    (output, newState) = runCommand executeGet expr gw

                output `shouldBe` "Moved silver coin to Alice the Adventurer"
                checkItemTagInPocket "silver coin" newState `shouldBe` True

            it "handles binary expression (get <item> from <location>)" $ do
                let gw = defaultGW
                    expr = BinaryExpression "get" (PrepClause "from") (NounClause "silver coin")
                    (output, newState) = runCommand executeGet expr gw

                output `shouldBe` "TO BE FIXED: binary expression not yet supported"
                checkItemTagInPocket "silver coin" newState `shouldBe` False

            it "handles complex expression (should be invalid)" $ do
                let gw = defaultGW
                    expr = ComplexExpression "get" (NounClause "all") (PrepClause "from") (NounClause "meadow")
                    (output, newState) = runCommand executeGet expr gw

                output `shouldBe` "TO BE FIXED: complex expression not supported for get"
                checkItemTagInPocket "silver coin" newState `shouldBe` False

        describe "item validation" $ do
            it "allows picking up items from current location" $ do
                let gw = defaultGW
                    expr = UnaryExpression "get" (NounClause "silver coin")
                    (output, newState) = runCommand executeGet expr gw

                output `shouldBe` "Moved silver coin to Alice the Adventurer"
                checkItemTagInPocket "silver coin" newState `shouldBe` True

            it "prevents picking up non-existent items" $ do
                let gw = defaultGW
                    expr = UnaryExpression "get" (NounClause "golden coin")
                    (output, newState) = runCommand executeGet expr gw

                output `shouldBe` "Cannot pick up \"TO BE FIXED\""
                checkItemTagInPocket "golden coin" newState `shouldBe` False

            it "prevents picking up items from different locations" $ do
                let gw = defaultGW  -- player starts in meadow
                    expr = UnaryExpression "get" (NounClause "bat")  -- bat is in cave
                    (output, newState) = runCommand executeGet expr gw

                output `shouldBe` "Cannot pick up \"TO BE FIXED\""
                checkItemTagInPocket "bat" newState `shouldBe` False

        describe "inventory management" $ do
            it "moves item from location to inventory" $ do
                let gw = defaultGW
                    expr = UnaryExpression "get" (NounClause "silver coin")
                    (_, newState) = runCommand executeGet expr gw

                -- Verify item is in inventory
                checkItemTagInPocket "silver coin" newState `shouldBe` True

                -- Verify item is no longer in location
                let itemsInMeadow = map getTag $ getItemsAtLoc testMeadow newState
                "silver coin" `elem` itemsInMeadow `shouldBe` False

            it "can pick up container items" $ do
                let gw = defaultGW
                    expr = UnaryExpression "get" (NounClause "bag of holding")
                    (output, newState) = runCommand executeGet expr gw

                output `shouldBe` "Moved bag of holding to Alice the Adventurer"
                checkItemTagInPocket "bag of holding" newState `shouldBe` True

            it "maintains container contents when picking up containers" $ do
                let gw = defaultGW
                    setupExpr = ComplexExpression "put" (NounClause "silver coin") (PrepClause "in") (NounClause "bag of holding")
                    (_, setupState) = runCommand executePut setupExpr gw

                    -- Then pick up the bag
                    getExpr = UnaryExpression "get" (NounClause "bag of holding")
                    (_, finalState) = runCommand executeGet getExpr setupState

                -- Verify the item is still in the bag after moving it
                case findItemByTag "bag of holding" finalState >>= getInventory of
                    Just loc ->
                        let itemsInBag = map getTag $ getItemsAtLoc loc finalState
                        in "silver coin" `elem` itemsInBag `shouldBe` True
                    Nothing ->
                        expectationFailure "Bag lost its inventory location"