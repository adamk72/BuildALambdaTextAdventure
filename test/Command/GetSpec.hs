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

        describe "item validation" $ do
            it "allows picking up items from current location" $ do
                let gw = defaultGW
                    expr = UnaryExpression "get" (NounClause "silver coin")
                (output, newState) <- runCommand executeGet expr gw

                output `shouldBe` "Moved silver coin to Alice the Adventurer"
                checkItemTagInPocket "silver coin" newState `shouldBe` True

            it "prevents picking up non-existent items in location" $ do
                let gw = defaultGW
                    expr = UnaryExpression "get" (NounClause "golden coin")
                (output, newState) <- runCommand executeGet expr gw

                output `shouldBe` renderMessage (InvalidItem "golden coin")
                checkItemTagInPocket "golden coin" newState `shouldBe` False

        describe "inventory management" $ do
            it "moves item from location to inventory" $ do
                let gw = defaultGW
                    expr = UnaryExpression "get" (NounClause "silver coin")
                (_, newState) <- runCommand executeGet expr gw

                -- Verify item is in inventory
                checkItemTagInPocket "silver coin" newState `shouldBe` True

                -- Verify item is no longer in location
                let itemsInMeadow = getItemTagsAtLoc testMeadow newState
                "silver coin" `elem` itemsInMeadow `shouldBe` False

            it "can pick up container items" $ do
                let gw = defaultGW
                    expr = UnaryExpression "get" (NounClause "bag of holding")
                (output, newState) <- runCommand executeGet expr gw

                output `shouldBe` "Moved bag of holding to Alice the Adventurer"
                checkItemTagInPocket "bag of holding" newState `shouldBe` True

            it "maintains container contents when picking up containers" $ do
                let gw = defaultGW
                    setupExpr = ComplexExpression "put" (NounClause "silver coin") (PrepClause "in") (NounClause "bag of holding")
                (_, setupState) <- runCommand executePut setupExpr gw

                    -- Then pick up the bag
                let getExpr = UnaryExpression "get" (NounClause "bag of holding")
                (_, finalState) <- runCommand executeGet getExpr setupState

                -- Verify the item is still in the bag after moving it
                case findEntityById "bag of holding" finalState >>= getInventory of
                    Just loc ->
                        let itemsInBag = getItemTagsAtLoc loc finalState
                        in "silver coin" `elem` itemsInBag `shouldBe` True
                    Nothing ->
                        expectationFailure "Bag lost its inventory location"
        describe "container interactions" $ do
                    it "allows getting items from containers in the same location" $ do
                        let gw = defaultGW
                            expr = UnaryExpression "get" (NounClause "pearl")
                        (output, newState) <- runCommand executeGet expr gw

                        output `shouldBe` "Moved pearl to Alice the Adventurer"
                        checkItemTagInPocket "pearl" newState `shouldBe` True

                        -- Verify the item is no longer in the container
                        case findEntityById "bag of holding" newState >>= getInventory of
                            Just loc -> do
                                let itemsInBag = getItemTagsAtLoc loc newState
                                "pearl" `elem` itemsInBag `shouldBe` False
                            Nothing -> expectationFailure "Bag lost its inventory location"

                    it "allows getting multiple items from the same container" $ do
                        let gw = defaultGW
                            expr1 = UnaryExpression "get" (NounClause "pearl")
                            expr2 = UnaryExpression "get" (NounClause "another pearl")

                        (_, midState) <- runCommand executeGet expr1 gw
                        (output, finalState) <- runCommand executeGet expr2 midState

                        output `shouldBe` "Moved another pearl to Alice the Adventurer"
                        checkItemTagInPocket "pearl" finalState `shouldBe` True
                        checkItemTagInPocket "another pearl" finalState `shouldBe` True

                    it "prevents getting items from containers in other locations" $ do
                        let gw = defaultGW `withActorAt` testCave  -- Move actor to cave
                            expr = UnaryExpression "get" (NounClause "pearl")
                        (output, newState) <- runCommand executeGet expr gw

                        output `shouldBe` renderMessage (InvalidItem "pearl")
                        checkItemTagInPocket "pearl" newState `shouldBe` False

                    it "prevents getting the container and its contents simultaneously" $ do
                        let gw = defaultGW
                            expr = UnaryExpression "get" (NounClause "bag of holding")
                        (output, newState) <- runCommand executeGet expr gw

                        output `shouldBe` "Moved bag of holding to Alice the Adventurer"

                        -- Verify container is in inventory but pearls remain inside
                        checkItemTagInPocket "bag of holding" newState `shouldBe` True
                        case findEntityById "bag of holding" newState >>= getInventory of
                            Just loc -> do
                                let itemsInBag = getItemTagsAtLoc loc newState
                                "pearl" `elem` itemsInBag `shouldBe` True
                                "another pearl" `elem` itemsInBag `shouldBe` True
                            Nothing -> expectationFailure "Bag lost its inventory location"

                    it "allows getting items from containers owned by other actors but in same location" $ do
                        -- Setup: Move a container to Bob's inventory first
                        let gw = defaultGW
                            updatedGW = moveItemLoc testBagOfHolding (fromJust $ getInventory $ head $ gwPlayableActors gw) gw
                            expr = UnaryExpression "get" (NounClause "pearl")

                        (output, newState) <- runCommand executeGet expr updatedGW

                        output `shouldBe` "Moved pearl to Alice the Adventurer"
                        checkItemTagInPocket "pearl" newState `shouldBe` True
