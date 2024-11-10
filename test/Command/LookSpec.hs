{-# LANGUAGE OverloadedStrings #-}
module Command.LookSpec (spec) where

import           Command.Commands
import           Command.TestUtils
import           Core.State
import           Mock.GameEnvironment
import           Parser.Types
import           Test.Hspec

spec :: Spec
spec = do
    describe "Look Command" $ do
        describe "expression handling" $ do
            it "handles atomic expression (just 'look')" $ do
                let gw = defaultGW
                    expr = AtomicExpression "look"
                (output, newState) <- runCommand executeLook expr gw

                output `shouldBe` "You are in a flowery meadow."
                verifyStartLocation newState "meadow"

            it "handles unary expression (look around)" $ do
                let gw = defaultGW
                    expr = UnaryExpression "look" (NounClause "around")
                (output, newState) <- runCommand executeLook expr gw

                output `shouldBe` "You are in a flowery meadow. You look around and see a silver coin, a bag of holding, a simple bag, and a shiny bauble."
                verifyStartLocation newState "meadow"

            it "handles binary expression (look at <thing>)" $ do
                let gw = defaultGW
                    expr = BinaryExpression "look" (PrepClause "at") (NounClause "silver coin")
                (output, newState) <- runCommand executeLook expr gw

                output `shouldBe` "TO BE FIXED: detailed item examination not implemented"
                verifyStartLocation newState "meadow"

            it "handles binary expression (look towards <direction>)" $ do
                let gw = defaultGW
                    expr = BinaryExpression "look" (PrepClause "towards") (NounClause "cave")
                (output, newState) <- runCommand executeLook expr gw

                output `shouldBe` "TO BE FIXED: directional looking not implemented"
                verifyStartLocation newState "meadow"

            it "handles complex expression (should be invalid)" $ do
                let gw = defaultGW
                    expr = ComplexExpression "look" (NounClause "carefully") (PrepClause "at") (NounClause "silver coin")
                (output, newState) <- runCommand executeLook expr gw

                output `shouldBe` "TO BE FIXED: complex look expressions not supported"
                verifyStartLocation newState "meadow"

        describe "location descriptions" $ do
            it "shows basic location description when just looking" $ do
                let gw = defaultGW
                    expr = AtomicExpression "look"
                (output, _) <- runCommand executeLook expr gw

                output `shouldBe` "You are in a flowery meadow."

            it "shows location and items when looking around in meadow" $ do
                let gw = defaultGW
                    expr = UnaryExpression "look" (NounClause "around")
                (output, _) <- runCommand executeLook expr gw

                output `shouldBe` "You are in a flowery meadow. You look around and see a silver coin, a simple bag, a shiny bauble, and bag of holding."

            it "shows location and items when looking around in cave" $ do
                let gw = defaultGW `withActorAt` testCave
                    expr = UnaryExpression "look" (NounClause "around")
                (output, _) <- runCommand executeLook expr gw

                output `shouldBe` "You are in a dark cave. You look around and see a cute bat."

            it "shows appropriate message when no items are visible" $ do
                let gw = defaultGW { gwItems = [] }
                    expr = UnaryExpression "look" (NounClause "around")
                (output, _) <- runCommand executeLook expr gw

                output `shouldBe` "You are in a flowery meadow. You look around and see ."

        describe "container examination" $ do
            it "shows content of a container with single item" $ do
                let gw = defaultGW  -- includes testPearl
                    expr = BinaryExpression "look" (PrepClause "in") (NounClause "bag of holding")
                (output, newState) <- runCommand executeLook expr gw

                output `shouldBe` "Inside the bag of holding you see a pearl of unique luster."
                verifyStartLocation newState "meadow"

                -- Verify pearl is still in the bag
                case findItemByTag "bag of holding" newState >>= getInventory of
                    Just loc -> do
                        let itemsInBag = getItemTagsAtLoc loc newState
                        itemsInBag `shouldContain` ["pearl"]
                    Nothing -> expectationFailure "Bag lost its inventory location"

            it "shows multiple items in container after adding item" $ do
                -- First get the bauble (since it starts in the meadow)
                let gw = defaultGW
                    getExpr = UnaryExpression "get" (NounClause "bauble")
                (_, state1) <- runCommand executeGet getExpr gw

                -- Then put the bauble in the bag
                let putExpr = ComplexExpression "put" (NounClause "bauble") (PrepClause "in") (NounClause "bag of holding")
                (_, state2) <- runCommand executePut putExpr state1

                -- Finally look in the container
                let lookExpr = BinaryExpression "look" (PrepClause "in") (NounClause "bag of holding")
                (output, finalState) <- runCommand executeLook lookExpr state2

                -- Use shouldSatisfy to check content without depending on order
                output `shouldSatisfy` (\s -> s == "Inside the bag of holding you see a pearl of unique luster and a shiny bauble." ||
                                            s == "Inside the bag of holding you see a shiny bauble and a pearl of unique luster.")
                verifyStartLocation finalState "meadow"

                -- Verify both items are still in the bag
                case findItemByTag "bag of holding" finalState >>= getInventory of
                    Just loc -> do
                        let itemsInBag = getItemTagsAtLoc loc finalState
                        itemsInBag `shouldContain` ["pearl"]
                        itemsInBag `shouldContain` ["bauble"]
                    Nothing -> expectationFailure "Bag lost its inventory location"

        describe "state preservation" $ do
            it "maintains actor location after looking" $ do
                let gw = defaultGW
                    expr = UnaryExpression "look" (NounClause "around")
                (_, newState) <- runCommand executeLook expr gw

                verifyStartLocation newState "meadow"

            it "maintains inventory after looking" $ do
                let gw = defaultGW
                    expr = UnaryExpression "look" (NounClause "around")
                (_, newState) <- runCommand executeLook expr gw
                let originalInventory = getActorInventoryItems gw
                    newInventory = getActorInventoryItems newState

                newInventory `shouldMatchList` originalInventory

            it "maintains item locations after looking" $ do
                let gw = defaultGW
                    expr = UnaryExpression "look" (NounClause "around")
                (_, newState) <- runCommand executeLook expr gw
                let originalItems = gwItems gw
                    newItems = gwItems newState

                map (getLocation) newItems `shouldMatchList` map (getLocation) originalItems

            it "maintains container contents after looking" $ do
                -- First put something in a container
                let gw = defaultGW
                    setupExpr = ComplexExpression "put" (NounClause "silver coin") (PrepClause "in") (NounClause "bag of holding")
                (_, setupState) <- runCommand executePut setupExpr gw

                    -- Then look around
                let lookExpr = UnaryExpression "look" (NounClause "around")
                (_, finalState) <- runCommand executeLook lookExpr setupState

                case findItemByTag "bag of holding" finalState >>= getInventory of
                    Just loc ->
                        let itemsInBag = getItemTagsAtLoc loc finalState
                        in "silver coin" `elem` itemsInBag `shouldBe` True
                    Nothing ->
                        expectationFailure "Bag lost its inventory location"
