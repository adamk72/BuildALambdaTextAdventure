{-# LANGUAGE OverloadedStrings #-}
module Command.LookSpec (spec) where

import           Command.Commands
import           Command.TestUtils
import           Core.State
import           Mock.GameEnvironment
import           Test.Hspec
import           Parser.Types

spec :: Spec
spec = do
    describe "Look Command" $ do
        describe "basic looking" $ do
            it "shows current location when just looking" $ do
                let gw = defaultGW
                    expr = AtomicExpression "look"
                    (output, _) = runCommand executeLook expr gw

                output `shouldBe` "You are in a flowery meadow."

        describe "looking around" $ do
            it "shows location and visible items when looking around" $ do
                let gw = defaultGW
                    expr = UnaryExpression "look" (NounClause "around")
                    (output, _) = runCommand executeLook expr gw

                output `shouldBe` "You are in a flowery meadow. You look around and see a sliver coin, a bag of holding, a simple bag, and a shiny bauble."

            it "shows appropriate message when no items are visible" $ do
                let gw = defaultGW { gwItems = [] }
                    expr = UnaryExpression "look" (NounClause "around")
                    (output, _) = runCommand executeLook expr gw

                output `shouldBe` "You are in a flowery meadow. You look around and see ."
