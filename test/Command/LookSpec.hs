{-# LANGUAGE OverloadedStrings #-}
module Command.LookSpec (spec) where

import           Command.Commands
import           Command.TestUtils
import           Core.State
import           Core.Operations
import           Mock.TestWorld
import           Parser.Types
import           Test.Hspec
import           Entity.Entity
import           Data.Text            (Text, isInfixOf)
import qualified Data.Map            as Map
import           Command.Message

spec :: Spec
spec = do
    describe "Look Command" $ do
        describe "expression handling" $ do
            it "handles atomic expression (just 'look')" $ do
                let gw = defaultGW
                    expr = AtomicCmdExpression "look"
                (output, newState) <- runCommand executeLook expr gw

                output `shouldBe` "A general view of the space and possibly some items."
                verifyStartLocation newState "meadow"

            it "handles unary expression (look around)" $ do
                let gw = defaultGW
                    expr = UnaryCmdExpression "look" (NounClause "around")
                (output, newState) <- runCommand executeLook expr gw

                -- Test that output includes location and visible items
                "flowery meadow" `shouldSatisfy` (`isInfixOf` output)
                "silver coin" `shouldSatisfy` (`isInfixOf` output)
                "bag of holding" `shouldSatisfy` (`isInfixOf` output)
                verifyStartLocation newState "meadow"

            it "handles looking at specific items" $ do
                let gw = defaultGW
                    expr = BinaryCmdExpression "look" (PrepClause "at") (NounClause "silver coin")
                (output, newState) <- runCommand executeLook expr gw

                "silver coin" `shouldSatisfy` (`isInfixOf` output)
                verifyStartLocation newState "meadow"

            it "handles looking in containers" $ do
                let gw = defaultGW
                    expr = BinaryCmdExpression "look" (PrepClause "in") (NounClause "bag of holding")
                (output, newState) <- runCommand executeLook expr gw

                "pearl" `shouldSatisfy` (`isInfixOf` output)
                verifyStartLocation newState "meadow"

        describe "visibility rules" $ do
            it "shows items in the current location" $ do
                let gw = defaultGW
                    expr = UnaryCmdExpression "look" (NounClause "around")
                (output, _) <- runCommand executeLook expr gw
                let visibleMovables = getViewableNamesAtActorLoc gw

                "silver coin" `shouldSatisfy` (`isInfixOf` output)
                "bag of holding" `shouldSatisfy` (`isInfixOf` output)

            it "doesn't show items in other locations" $ do
                let gw = defaultGW
                    expr = UnaryCmdExpression "look" (NounClause "around")
                (output, _) <- runCommand executeLook expr gw

                "bat" `shouldNotSatisfy` (`isInfixOf` output)  -- bat is in cave
                "eight ball" `shouldNotSatisfy` (`isInfixOf` output)  -- eight ball is in forest

            it "shows items in visible containers" $ do
                let gw = defaultGW
                    expr = BinaryCmdExpression "look" (PrepClause "in") (NounClause "bag of holding")
                (output, _) <- runCommand executeLook expr gw

                "pearl" `shouldSatisfy` (`isInfixOf` output)

        describe "state preservation" $ do
            it "maintains actor location after looking" $ do
                let gw = defaultGW
                    expr = UnaryCmdExpression "look" (NounClause "around")
                (_, newState) <- runCommand executeLook expr gw

                verifyStartLocation newState "meadow"

            it "maintains inventory after looking" $ do
                let gw = defaultGW
                    expr = UnaryCmdExpression "look" (NounClause "around")
                (_, newState) <- runCommand executeLook expr gw

                let originalInventory = getActiveActorInventoryList gw
                    newInventory = getActiveActorInventoryList newState

                map getId newInventory `shouldMatchList` map getId originalInventory

            it "maintains world state integrity" $ do
                let gw = defaultGW
                    expr = UnaryCmdExpression "look" (NounClause "around")
                (_, newState) <- runCommand executeLook expr gw

                Map.size (items gw) `shouldBe` Map.size (items newState)
                Map.size (locations gw) `shouldBe` Map.size (locations newState)
                Map.size (actors gw) `shouldBe` Map.size (actors newState)