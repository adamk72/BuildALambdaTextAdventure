{-# LANGUAGE OverloadedStrings #-}
module Command.PutSpec (spec) where

import           Command.Commands
import           Command.TestUtils
import           Core.Message
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
                    expr = AtomicExpression "put"
                (output, newState) <- runCommand executePut expr gw

                output `shouldBe` renderMessage PutWhat
                verifyStartLocation newState "meadow"

            it "handles unary expression (put <item>)" $ do
                let gw = defaultGW
                    expr = UnaryExpression "put" (NounClause "silver coin")
                (output, newState) <- runCommand executePut expr gw

                output `shouldBe` renderMessage (PutWhere "silver coin")
                verifyStartLocation newState "meadow"

            it "handles binary expression (put in <container>)" $ do
                let gw = defaultGW
                    expr = BinaryExpression "put" (PrepClause "in") (NounClause "bag of holding")
                (output, newState) <- runCommand executePut expr gw

                output `shouldBe` renderMessage PutWhat
                verifyStartLocation newState "meadow"

            it "handles complex expression (put <item> in <container>)" $ do
                let gw = defaultGW
                    expr = ComplexExpression "put" (NounClause "silver coin") (PrepClause "in") (NounClause "bag of holding")
                (output, newState) <- runCommand executePut expr gw

                output `shouldBe` "silver coin is now in the bag of holding."
                case findEntityById "bag of holding" newState >>= getInventory of
                    Just loc ->
                        let itemsInBag = getItemTagsAtLoc loc newState
                        in "silver coin" `elem` itemsInBag `shouldBe` True
                    Nothing ->
                        expectationFailure "Bag lost its inventory location"

        describe "container validation" $ do
            it "allows putting items in valid containers" $ do
                let gw = defaultGW
                    expr = ComplexExpression "put" (NounClause "silver coin") (PrepClause "in") (NounClause "bag of holding")
                (output, newState) <- runCommand executePut expr gw

                output `shouldBe` "silver coin is now in the bag of holding."
                case findEntityById "bag of holding" newState >>= getInventory of
                    Just loc ->
                        let itemsInBag = getItemTagsAtLoc loc newState
                        in "silver coin" `elem` itemsInBag `shouldBe` True
                    Nothing ->
                        expectationFailure "Bag lost its inventory location"

            it "prevents putting items in non-containers" $ do
                let gw = defaultGW
                    expr = ComplexExpression "put" (NounClause "silver coin") (PrepClause "in") (NounClause "bauble")
                (output, _) <- runCommand executePut expr gw

                output `shouldBe` "The bauble is not a container."

            it "prevents putting non-existent items in containers" $ do
                let gw = defaultGW
                    expr = ComplexExpression "put" (NounClause "golden coin") (PrepClause "in") (NounClause "bag of holding")
                (output, _) <- runCommand executePut expr gw

                output `shouldBe` renderMessage (NoItemForContainer "golden coin" "bag of holding")

            it "prevents putting items in non-existent containers" $ do
                let gw = defaultGW
                    updatedGW = moveItemLoc testCoin testMeadow gw
                    expr = ComplexExpression "put" (NounClause "silver coin") (PrepClause "in") (NounClause "magic box")
                (output, _) <- runCommand executePut expr updatedGW

                output `shouldBe` renderMessage (NoContainerForItem "silver coin" "magic box")

        describe "state management" $ do
            it "moves item from location to container" $ do
                let gw = defaultGW
                    updatedGW = moveItemLoc testCoin testMeadow gw
                    expr = ComplexExpression "put" (NounClause "silver coin") (PrepClause "in") (NounClause "bag of holding")
                (_, newState) <- runCommand executePut expr updatedGW

                -- Verify item is in container
                case findEntityById "bag of holding" newState >>= getInventory of
                    Just loc ->
                        let itemsInBag = getItemTagsAtLoc loc newState
                        in "silver coin" `elem` itemsInBag `shouldBe` True
                    Nothing ->
                        expectationFailure "Bag lost its inventory location"

                -- Verify item is not in original location
                let itemsInMeadow = getItemTagsAtLoc testMeadow newState
                "silver coin" `elem` itemsInMeadow `shouldBe` False

            it "moves item from inventory to container" $ do
                -- First get the item
                let gw = defaultGW
                    updatedGW = moveItemLoc testCoin testMeadow gw
                    getExpr = UnaryExpression "get" (NounClause "silver coin")
                (_, midState) <- runCommand executeGet getExpr updatedGW

                    -- Then put it in container
                let putExpr = ComplexExpression "put" (NounClause "silver coin") (PrepClause "in") (NounClause "bag of holding")
                (_, finalState) <- runCommand executePut putExpr midState

                -- Verify item is in container
                case findEntityById "bag of holding" finalState >>= getInventory of
                    Just loc ->
                        let itemsInBag = getItemTagsAtLoc loc finalState
                        in "silver coin" `elem` itemsInBag `shouldBe` True
                    Nothing ->
                        expectationFailure "Bag lost its inventory location"

                -- Verify item is not in inventory
                checkItemTagInPocket "silver coin" finalState `shouldBe` False
