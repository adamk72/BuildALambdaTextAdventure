module DistanceSpec (spec) where

-- import           Command.Get
-- import           Command.TestUtils
-- import           Core.State
-- import           Core.Distance
-- import           Data.Maybe
-- import           Mock.GameEnvironment
import           Test.Hspec

spec :: Spec
spec = describe "Basic Function Checks" $ do
  it "" $ do
    True `shouldBe` True
  -- let ac = gwActiveActor defaultGW
  -- context "Distances between the actor and an object" $ do
  --   it "is the actor" $ do
  --     getDistance ac ac `shouldBe` DistanceSelf
  --   it "is in location near the actor the actor" $ do
  --     getDistance ac testBat `shouldBe` DistanceOverThere
  --   it "is not near the actor" $ do
  --     getDistance ac testEightBall `shouldBe` DistanceNotHere
  -- context "Picking up an object " $ do
  --   it "is in the same place as the actor" $ do
  --     getDistance ac testCoin `shouldBe` DistanceHere
  --   it "has been picked up by the actor" $ do
  --     let (_, coinGW)= runCommand executeGet "silver coin" defaultGW
  --         acWithCoin = gwActiveActor coinGW
  --         newCoin = fromJust (findItemByTag "silver coin" coinGW)
  --     -- Todo: need more to transfer to the bag
  --     getDistance acWithCoin newCoin `shouldBe` DistanceHeld
  --   it "is nested in a bag of holding in the actor's inventory" $ do
  --     let (_, bagGW)= runCommand executeGet "bag of holding" defaultGW
  --         acWithBag = gwActiveActor bagGW
  --         newBag = fromJust (findItemByTag "bag of holding" bagGW)
  --     getDistance acWithBag newBag `shouldBe` DistanceHeldContained
  -- context "Between non-actor objects" $ do
  --   it "doesn't have a pocket slot" $ do
  --     getDistance testCoin testBat `shouldBe` DistanceNotHere



