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
        describe "basic dropping" $ do
            it "allows dropping held items" $ do
                let gw = defaultGW -- First need to get an item
                    expr = UnaryExpression "drop" (NounClause "silver coin")
                    (output, newState) = runCommand executeDrop expr gw

                output `shouldBe` "silver coin dropped. Your inventory is now: "
                checkItemTagInPocket "silver coin" newState `shouldBe` False

            it "prevents dropping items not in inventory" $ do
                let gw = defaultGW
                    expr = UnaryExpression "drop" (NounClause "nonexistent")
                    (output, newState) = runCommand executeDrop expr gw

                output `shouldBe` "You don't have a nonexistent to drop."
                verifyStartLocation newState "meadow" -- Location shouldn't change

        describe "inventory validation" $ do
            it "updates inventory correctly after dropping" $ do
                let gw = defaultGW -- First need to get an item
                    expr = UnaryExpression "drop" (NounClause "silver coin")
                    (_, newState) = runCommand executeDrop expr gw
                    inventoryItems = getActorInventoryItems newState

                length inventoryItems `shouldBe` 0
