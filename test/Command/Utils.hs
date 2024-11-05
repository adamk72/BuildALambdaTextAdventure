{-# LANGUAGE OverloadedStrings #-}
module Command.Utils (module Command.Utils) where

import           Control.Monad.State
import           Core.State
import           Data.Text            (Text)
import           Mock.GameEnvironment
import           Test.Hspec

-- Generic command runner that can be used for all commands
type CommandExecutor = Maybe Text -> State GameWorld Text

runCommand :: CommandExecutor -> Maybe Text -> GameWorld -> (Text, GameWorld)
runCommand executor cmd = runState (executor cmd)

-- Common test context helpers
verifyStartLocation :: GameWorld -> Text -> Expectation
verifyStartLocation gw expectedLoc = do
    let acLoc = locTag $ gwActiveActorLoc gw
    acLoc `shouldBe` expectedLoc

verifyStateUnchanged :: (Text, GameWorld) -> GameWorld -> Expectation
verifyStateUnchanged (_, newState) originalState =
    newState `shouldBe` originalState

-- Common test cases that can be reused across command specs
checkNoInputHandling :: CommandExecutor -> Text -> Spec
checkNoInputHandling executor expectedMsg =
    context "when given no input" $
        it "handles Nothing input" $ do
            let result = runCommand executor Nothing defaultGW
            verifyCommandResult result expectedMsg
            verifyStateUnchanged result defaultGW

verifyCommandResult :: (Text, GameWorld) -> Text -> Expectation
verifyCommandResult (result, _) expected =
    result `shouldBe` expected

