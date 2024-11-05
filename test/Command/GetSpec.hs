{-# LANGUAGE OverloadedStrings #-}
module Command.GetSpec (spec) where

import           Command.Get
import           Command.Common
import           Command.TestUtils
import           Core.State
import           Data.List            as List (find)
import           Data.Maybe           (fromJust)
import           Mock.GameEnvironment
import           Test.Hspec

spec :: Spec
spec = describe "executeGet" $ do
    let ac = gwActiveActor defaultGW
        startLoc = getActiveActorLoc defaultGW

    context "check testing assumptions" $ do
        it "should have the silver coin in the starting location" $ do
            let coin = List.find (\item -> getTag item == "silver coin") (gwItems defaultGW)
            fmap getLocation coin `shouldBe` Just startLoc

        it "should have the active character in the correct location" $ do
            verifyStartLocation defaultGW "meadow"

    context "when picking up objects" $ do
        let (_, getGW) = runCommand executeGet "silver coin" defaultGW
            coin = findItemByTag "silver coin" getGW
            expectedLoc = getPocketSlot getGW

        it "can transfer silver coin from location to person" $ do
            fmap getLocation coin `shouldBe` Just expectedLoc

        it "is no longer an element in the environment" $ do
            let objs = getItemsAtLoc (getLocation ac) getGW
            notElem (fromJust coin) objs `shouldBe` True

        it "handles attempting to get nonexistent objects" $ do
            let (result, newState) = runCommand executeGet "nonexistent" defaultGW
            result `shouldBe` renderMessage (InvalidItem "nonexistent")
            newState `shouldBe` defaultGW