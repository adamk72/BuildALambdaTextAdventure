{-# LANGUAGE OverloadedStrings #-}
module Command.GetSpec (spec) where

import           Command.Common
import           Command.Get
import           Command.Utils
import           Core.State
import           Data.List            as List (find)
import           Data.Maybe           (fromJust)
import           Mock.GameEnvironment
import           Test.Hspec

spec :: Spec
spec = describe "executeGet" $ do
    let ac = gwActiveActor defaultGW
        startLoc = gwActiveActorLoc defaultGW

    context "check testing assumptions" $ do
        it "should have the silver coin in the starting location" $ do
            let coin = List.find (\item -> getTag item == "silver coin") (gwItems defaultGW)
            fmap getLocation coin `shouldBe` Just startLoc

        it "should have the active character in the correct location" $ do
            verifyStartLocation defaultGW "meadow"

    context "when picking up objects" $ do
        let (_, getGW) = runCommand executeGet (Just "silver coin") defaultGW
            coin = findItemByTag "silver coin" getGW
            expectedLoc = getPocketSlot getGW
        it "can transfer silver coin from location to person" $ do
            fmap getLocation coin `shouldBe` Just expectedLoc

        it "is no longer an element in the environment" $ do
            let objs = getItemsAtLoc (getLocation ac) getGW
            notElem (fromJust coin) objs `shouldBe` True

    checkNoInputHandling executeGet $ renderMessage NoItemSpecified


        -- it "handles attempting to get nonexistent objects" $ do
        --     let (result, newState) = runGetCommand (Just "nonexistent") defaultGW
        --     result `shouldBe` renderMessage (ObjectNotFound "nonexistent")
        --     newState `shouldBe` defaultGW

    -- context "when given no object" $
    --     it "handles Nothing input" $ do
    --         let (result, newState) = runGetCommand Nothing defaultGW
    --         result `shouldBe` renderMessage NoObjectSpecified
    --         newState `shouldBe` defaultGW
