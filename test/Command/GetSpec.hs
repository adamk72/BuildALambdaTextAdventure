module Command.GetSpec (spec) where

import           Command.Get
import           Core.State.GameState
import           Control.Monad.State
import           Test.Hspec
import Data.Text
import Mock.GameEnvironment

runGetCommand :: Maybe Text -> GameWorld -> (Text, GameWorld)
runGetCommand cmd = runState (executeGet cmd)

spec :: Spec
spec = describe "test 'get' command" $ do
  context "picking up objects" $ do
    it "can transfer from location to person" $ do
      let _expectedGW = makeTestWorld (testAlice testMeadow) [testBob testMeadow] [testCave, testMeadow, testForest] testInteractables
          (result, _newState) = runGetCommand (Just "silver coin") defaultGW
      result `shouldBe` "bob"


