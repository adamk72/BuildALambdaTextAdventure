{-# LANGUAGE OverloadedStrings #-}
module Command.PutSpec (spec) where

import           Command.Commands
import           Command.TestUtils
import           Core.State
import           Data.Text
import           Mock.GameEnvironment
import           Parser.Parser
import           Parser.Types
import           Test.Hspec

spec :: Spec
spec = do
    describe "Put Command" $ do
        describe "basic item placement" $ do
            it "allows putting items in containers" $ do
                let gw = defaultGW
                    expr = ComplexExpression "put" (NounClause "bauble") (PrepClause "in") (NounClause "bag of holding")
                    (output, newState) = runCommand executePut expr gw

                output `shouldBe` "bauble is now in the bag of holding."
                checkItemLocation "bauble" "bag of holding" newState `shouldBe` True

            it "prevents putting items in non-containers" $ do
                let gw = defaultGW
                    expr = ComplexExpression "put" (NounClause "bauble") (PrepClause "in") (NounClause "silver coin")
                    (output, _) = runCommand executePut expr gw

                output `shouldBe` "The silver coin is not a container."

        describe "validation" $ do
            it "handles non-existent items" $ do
                let gw = defaultGW
                    expr = ComplexExpression "put" (NounClause "nonexistent") (PrepClause "in") (NounClause "bag of holding")
                    (output, _) = runCommand executePut expr gw

                output `shouldBe` "Location does not exist in this game world: nonexistent."

            it "handles non-existent containers" $ do
                let gw = defaultGW
                    expr = ComplexExpression "put" (NounClause "bauble") (PrepClause "in") (NounClause "nonexistent")
                    (output, _) = runCommand executePut expr gw

                output `shouldBe` "Location does not exist in this game world: nonexistent."

-- Helper function for checking item location
checkItemLocation :: Text -> Text -> GameWorld -> Bool
checkItemLocation itemTag containerTag gw =
    case findItemByTag itemTag gw of
        Just item -> case findItemByTag containerTag gw >>= getInventory of
            Just loc -> getLocation item == loc
            Nothing  -> False
        Nothing -> False
