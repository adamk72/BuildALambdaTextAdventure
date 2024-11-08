{-# LANGUAGE OverloadedStrings #-}
module Command.PutSpec (spec) where

import           Command.Commands
import           Command.TestUtils
import           Core.State
import           Mock.GameEnvironment
import           Test.Hspec
import           Parser.Types

spec :: Spec
spec = do
    describe "Put Command" $ do
        describe "expression handling" $ do
            it "handles atomic expression (just 'put')" $ do
                let gw = defaultGW
                    expr = AtomicExpression "put"
                    (output, newState) = runCommand executePut expr gw

                output `shouldBe` "TO BE FIXED: put requires an item and a container"
                verifyStartLocation newState "meadow"

            it "handles unary expression (put <item>)" $ do
                let gw = defaultGW
                    expr = UnaryExpression "put" (NounClause "silver coin")
                    (output, newState) = runCommand executePut expr gw

                output `shouldBe` "TO BE FIXED: put requires a container destination"
                verifyStartLocation newState "meadow"

            it "handles binary expression (put in <container>)" $ do
                let gw = defaultGW
                    expr = BinaryExpression "put" (PrepClause "in") (NounClause "bag of holding")
                    (output, newState) = runCommand executePut expr gw

                output `shouldBe` "TO BE FIXED: put requires an item to move"
                verifyStartLocation newState "meadow"

            it "handles complex expression (put <item> in <container>)" $ do
                let gw = defaultGW
                    expr = ComplexExpression "put" (NounClause "silver coin") (PrepClause "in") (NounClause "bag of holding")
                    (output, newState) = runCommand executePut expr gw

                output `shouldBe` "silver coin is now in the bag of holding."
                case findItemByTag "bag of holding" newState >>= getInventory of
                    Just loc ->
                        let itemsInBag = map getTag $ getItemsAtLoc loc newState
                        in "silver coin" `elem` itemsInBag `shouldBe` True
                    Nothing ->
                        expectationFailure "Bag lost its inventory location"

        describe "container validation" $ do
            it "allows putting items in valid containers" $ do
                let gw = defaultGW
                    expr = ComplexExpression "put" (NounClause "silver coin") (PrepClause "in") (NounClause "bag of holding")
                    (output, newState) = runCommand executePut expr gw

                output `shouldBe` "silver coin is now in the bag of holding."
                case findItemByTag "bag of holding" newState >>= getInventory of
                    Just loc ->
                        let itemsInBag = map getTag $ getItemsAtLoc loc newState
                        in "silver coin" `elem` itemsInBag `shouldBe` True
                    Nothing ->
                        expectationFailure "Bag lost its inventory location"

            it "prevents putting items in non-containers" $ do
                let gw = defaultGW
                    expr = ComplexExpression "put" (NounClause "silver coin") (PrepClause "in") (NounClause "bauble")
                    (output, _) = runCommand executePut expr gw

                output `shouldBe` "The bauble is not a container."

            it "prevents putting non-existent items in containers" $ do
                let gw = defaultGW
                    expr = ComplexExpression "put" (NounClause "golden coin") (PrepClause "in") (NounClause "bag of holding")
                    (output, _) = runCommand executePut expr gw

                output `shouldBe` "Location does not exist in this game world: golden coin."

            it "prevents putting items in non-existent containers" $ do
                let gw = defaultGW
                    expr = ComplexExpression "put" (NounClause "silver coin") (PrepClause "in") (NounClause "magic box")
                    (output, _) = runCommand executePut expr gw

                output `shouldBe` "Location does not exist in this game world: magic box."

        describe "state management" $ do
            it "moves item from location to container" $ do
                let gw = defaultGW
                    expr = ComplexExpression "put" (NounClause "silver coin") (PrepClause "in") (NounClause "bag of holding")
                    (_, newState) = runCommand executePut expr gw

                -- Verify item is in container
                case findItemByTag "bag of holding" newState >>= getInventory of
                    Just loc ->
                        let itemsInBag = map getTag $ getItemsAtLoc loc newState
                        in "silver coin" `elem` itemsInBag `shouldBe` True
                    Nothing ->
                        expectationFailure "Bag lost its inventory location"

                -- Verify item is not in original location
                let itemsInMeadow = map getTag $ getItemsAtLoc testMeadow newState
                "silver coin" `elem` itemsInMeadow `shouldBe` False

            it "moves item from inventory to container" $ do
                -- First get the item
                let gw = defaultGW
                    getExpr = UnaryExpression "get" (NounClause "silver coin")
                    (_, midState) = runCommand executeGet getExpr gw

                    -- Then put it in container
                    putExpr = ComplexExpression "put" (NounClause "silver coin") (PrepClause "in") (NounClause "bag of holding")
                    (_, finalState) = runCommand executePut putExpr midState

                -- Verify item is in container
                case findItemByTag "bag of holding" finalState >>= getInventory of
                    Just loc ->
                        let itemsInBag = map getTag $ getItemsAtLoc loc finalState
                        in "silver coin" `elem` itemsInBag `shouldBe` True
                    Nothing ->
                        expectationFailure "Bag lost its inventory location"

                -- Verify item is not in inventory
                checkItemTagInPocket "silver coin" finalState `shouldBe` False