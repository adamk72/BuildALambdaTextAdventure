{-# LANGUAGE OverloadedStrings #-}
module Command.PutSpec (spec) where

import           Command.Put
import           Command.Common
import           Command.TestUtils
import           Core.State
import           Data.List            as List (find)
import           Data.Maybe           (fromJust)
import           Mock.GameEnvironment
import           Test.Hspec

spec :: Spec
spec = describe "executePut" $ do
    let ac = gwActiveActor defaultGW
        startLoc = getActiveActorLoc defaultGW
        coin = findItemByTag "silver coin" defaultGW
        bag = findItemByTag "bag of holding" defaultGW

    context "check testing assumptions" $ do
        it "should have the silver coin in the starting location" $ do
            fmap getLocation coin `shouldBe` Just startLoc
        it "should have a bag of holding at the starting location" $ do
            fmap getLocation bag `shouldBe` Just startLoc

    context "putting a coin in the bag" $ do
        let (_, putGW) = runCommand executePut "silver coin in bag of holding" defaultGW
            coin = findItemByTag "silver coin" putGW

        it "can transfer silver coin from location to bag of holding" $ do
            fmap getLocation coin `shouldBe` getInventory testBagOfHolding

    --     it "is no longer an element in the environment" $ do
    --         let objs = getItemsAtLoc (getLocation ac) putGW
    --         notElem (fromJust coin) objs `shouldBe` True

    --     it "handles attempting to get nonexistent objects" $ do
    --         let (result, newState) = runCommand executePut "nonexistent" defaultGW
    --         result `shouldBe` renderMessage (InvalidItem "nonexistent")
    --         newState `shouldBe` defaultGW