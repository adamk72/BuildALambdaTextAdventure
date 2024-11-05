{-# LANGUAGE OverloadedStrings #-}
module Command.TestUtils (module Command.TestUtils) where

import           Control.Monad.State
import           Core.State
import           Data.Text            (Text)
import           Test.Hspec

-- Generic command runner that can be used for all commands
type CommandExecutor = Text -> State GameWorld Text

runCommand :: CommandExecutor -> Text -> GameWorld -> (Text, GameWorld)
runCommand executor cmd = runState (executor cmd)

-- Common test context helpers
verifyStartLocation :: GameWorld -> Text -> Expectation
verifyStartLocation gw expectedLoc = do
    let acLoc = locTag $ getActiveActorLoc gw
    acLoc `shouldBe` expectedLoc

