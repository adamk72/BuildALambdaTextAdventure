{-# LANGUAGE OverloadedStrings #-}
module Command.PutSpec (spec) where

import           Command.Put
import           Command.TestUtils
import           Core.State
import           Mock.GameEnvironment
import           Test.Hspec

spec :: Spec
spec = describe "executePut" $ do
    let startLoc = getActiveActorLoc defaultGW
        coin = findItemByTag "silver coin" defaultGW
        bagOfHolding = findItemByTag "bag of holding" defaultGW
        bag = findItemByTag "bag" defaultGW
        bauble = findItemByTag "bauble" defaultGW

    context "check testing assumptions" $ do
        it "should have the silver coin in the starting location" $ do
            fmap getLocation coin `shouldBe` Just startLoc
        it "should have a bag of holding at the starting location" $ do
            fmap getLocation bagOfHolding `shouldBe` Just startLoc
        it "should have a bag in the starting location" $ do
            fmap getLocation bag `shouldBe` Just startLoc
        it "should have a bag of holding at the starting location" $ do
            fmap getLocation bauble `shouldBe` Just startLoc

    context "check for basics by putting a bauble in a bag" $ do
        let (_, basicBagGW) = runCommand executePut "bauble in bag" defaultGW
            baggedBauble= findItemByTag "bauble" basicBagGW
        it "can put a bauble in a bag" $ do
            fmap getLocation baggedBauble `shouldBe` getInventory testBag

    context "check for spacing handling by putting a 'silver coin' in a 'bag of holding'" $ do
        let (_, putGW) = runCommand executePut "silver coin in bag of holding" defaultGW
            bagCoin = findItemByTag "silver coin" putGW
        it "can transfer silver coin from location to bag of holding" $ do
            fmap getLocation bagCoin `shouldBe` getInventory testBagOfHolding

    --     it "is no longer an element in the environment" $ do
    --         let objs = getItemsAtLoc (getLocation ac) putGW
    --         notElem (fromJust coin) objs `shouldBe` True

    --     it "handles attempting to get nonexistent objects" $ do
    --         let (result, newState) = runCommand executePut "nonexistent" defaultGW
    --         result `shouldBe` renderMessage (InvalidItem "nonexistent")
    --         newState `shouldBe` defaultGW