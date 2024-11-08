{-# LANGUAGE OverloadedStrings #-}
module Command.GetSpec (spec) where

import           Command.Commands
import           Command.TestUtils
import           Core.State
import           Mock.GameEnvironment
import           Test.Hspec
import           Parser.Types
import           Data.Text            (Text)

spec :: Spec
spec = do
    describe "Get Command" $ do
        describe "basic item pickup" $ do
            it "allows picking up items from current location" $ do
                let gw = defaultGW
                    cmd = "get silver coin"
                    (output, newState) = runCommand executeGet (UnaryExpression "get" (NounClause "silver coin")) gw
                    expectedOutput = "Moved silver coin to Alice the Adventurer"

                output `shouldBe` expectedOutput
                checkItemTagInPocket "silver coin" newState `shouldBe` True

            it "prevents picking up non-existent items" $ do
                let gw = defaultGW
                    cmd = "get gold coin"
                    (output, newState) = runCommand executeGet (UnaryExpression "get" (NounClause "gold coin")) gw

                output `shouldBe` "Cannot pick up \"PENDING\"."
                checkItemTagInPocket "gold coin" newState `shouldBe` False

        describe "location validation" $ do
            it "prevents picking up items from other locations" $ do
                let gw = defaultGW
                    cmd = "get bat"  -- bat is in cave, player starts in meadow
                    (output, newState) = runCommand executeGet (UnaryExpression "get" (NounClause "bat")) gw

                output `shouldBe` "Cannot pick up \"PENDING\"."
                checkItemTagInPocket "bat" newState `shouldBe` False
