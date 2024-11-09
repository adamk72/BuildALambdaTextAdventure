{-# LANGUAGE OverloadedStrings #-}
module Command.DropSpec (spec) where

import           Command.Commands
import           Command.TestUtils
import           Core.State
import           Mock.GameEnvironment
import           Test.Hspec
import           Parser.Types

spec :: Spec
spec = do
    describe "Drop Command" $ do
        describe "expression handling" $ do
            it "handles atomic expression (just 'drop')" $ do
                let gw = defaultGW
                    expr = AtomicExpression "drop"
                    (output, newState) = runCommand executeDrop expr gw

                output `shouldBe` renderMessage DropWhat
                verifyStartLocation newState "meadow"

            it "handles unary expression (drop <item>)" $ do
                -- First get the item
                let gw = defaultGW
                    getExpr = UnaryExpression "get" (NounClause "silver coin")
                    (_, midState) = runCommand executeGet getExpr gw

                    -- Then drop it
                    dropExpr = UnaryExpression "drop" (NounClause "silver coin")
                    (output, finalState) = runCommand executeDrop dropExpr midState

                output `shouldBe` renderMessage (DroppedItemWithInventory "silver coin" "") -- no inventory at this time
                checkItemTagInPocket "silver coin" finalState `shouldBe` False

            it "questions what is meant by an incomplete phrase" $ do
                let gw = defaultGW
                    expr = BinaryExpression "drop" (PrepClause "on") (NounClause "ground")
                    (output, _) = runCommand executeDrop expr gw
                output `shouldBe` renderMessage DropWhat

            it "handles full phrase" $ do
                let gw = defaultGW
                    expr = ComplexExpression "drop" (NounClause "silver coin") (PrepClause "on") (NounClause "ground")
                    (output, newState) = runCommand executeDrop expr gw

                output `shouldBe` renderMessage (DroppedItemSomewhere "silver coin" "on ground") -- no inventory at this time
                checkItemTagInPocket "silver coin" newState `shouldBe` False

            it "handles dropping of all objects" $ do
                let gw = defaultGW
                    expr = ComplexExpression "drop" (NounClause "all") (PrepClause "on") (NounClause "ground")
                    (output, newState) = runCommand executeDrop expr gw

                output `shouldBe` "IN DEVELOPMENT: dropping all items from inventory"
                verifyStartLocation newState "meadow"

        describe "inventory validation" $ do
            it "allows dropping items from inventory" $ do
                -- First get the item
                let gw = defaultGW
                    getExpr = UnaryExpression "get" (NounClause "silver coin")
                    (_, midState) = runCommand executeGet getExpr gw

                    -- Then drop it
                    dropExpr = UnaryExpression "drop" (NounClause "silver coin")
                    (output, finalState) = runCommand executeDrop dropExpr midState

                output `shouldBe` renderMessage (DroppedItemWithInventory "silver coin" "")
                checkItemTagInPocket "silver coin" finalState `shouldBe` False

            it "prevents dropping items not in inventory" $ do
                let gw = defaultGW
                    expr = UnaryExpression "drop" (NounClause "nonexistent")
                    (output, newState) = runCommand executeDrop expr gw

                output `shouldBe` "You don't have a nonexistent to drop."
                verifyStartLocation newState "meadow"

        describe "state management" $ do
            it "moves item from inventory to current location" $ do
                -- First get the item
                let gw = defaultGW
                    getExpr = UnaryExpression "get" (NounClause "silver coin")
                    (_, midState) = runCommand executeGet getExpr gw

                    -- Then drop it
                    dropExpr = UnaryExpression "drop" (NounClause "silver coin")
                    (_, finalState) = runCommand executeDrop dropExpr midState

                -- Verify item is in location
                let itemsInLoc = map getTag $ getItemsAtLoc testMeadow finalState
                "silver coin" `elem` itemsInLoc `shouldBe` True

                -- Verify item is not in inventory
                checkItemTagInPocket "silver coin" finalState `shouldBe` False

            it "maintains container contents when dropping containers" $ do
                -- First get container and put item in it
                let gw = defaultGW
                    getBagExpr = UnaryExpression "get" (NounClause "bag of holding")
                    (_, state1) = runCommand executeGet getBagExpr gw

                    getCoinExpr = UnaryExpression "get" (NounClause "silver coin")
                    (_, state2) = runCommand executeGet getCoinExpr state1

                    putExpr = ComplexExpression "put" (NounClause "silver coin") (PrepClause "in") (NounClause "bag of holding")
                    (_, state3) = runCommand executePut putExpr state2

                    -- Then drop the container
                    dropExpr = UnaryExpression "drop" (NounClause "bag of holding")
                    (_, finalState) = runCommand executeDrop dropExpr state3

                -- Verify container contents remain intact
                case findItemByTag "bag of holding" finalState >>= getInventory of
                    Just loc ->
                        let itemsInBag = map getTag $ getItemsAtLoc loc finalState
                        in "silver coin" `elem` itemsInBag `shouldBe` True
                    Nothing ->
                        expectationFailure "Bag lost its inventory location"

            it "updates location correctly when dropping in different locations" $ do
                -- First get the item in meadow
                let gw = defaultGW
                    getExpr = UnaryExpression "get" (NounClause "silver coin")
                    (_, state1) = runCommand executeGet getExpr gw

                    -- Move to cave
                    goExpr = UnaryExpression "go" (NounClause "cave")
                    (_, state2) = runCommand executeGo goExpr state1

                    -- Drop the item
                    dropExpr = UnaryExpression "drop" (NounClause "silver coin")
                    (_, finalState) = runCommand executeDrop dropExpr state2

                -- Verify item is in cave
                let itemsInCave = map getTag $ getItemsAtLoc testCave finalState
                "silver coin" `elem` itemsInCave `shouldBe` True

                -- Verify item is not in meadow
                let itemsInMeadow = map getTag $ getItemsAtLoc testMeadow finalState
                "silver coin" `elem` itemsInMeadow `shouldBe` False