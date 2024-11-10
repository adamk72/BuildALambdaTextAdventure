{-# LANGUAGE OverloadedStrings #-}
module Command.TestUtils (module Command.TestUtils) where

import           Command.CommandExecutor
import           Control.Monad.State
import           Core.State
import           Data.Text              (Text)
import           Logger                 (GameHistory, initGameHistory)
import           Parser.Types           (Expression)
import           System.Directory       (getTemporaryDirectory)
import           System.FilePath        ((</>))
import           Test.Hspec

-- | Create a test GameState with temporary log files
initTestState :: GameWorld -> IO GameState
initTestState world = do
    tmpDir <- getTemporaryDirectory
    let logPath = tmpDir </> "test.log"
        histPath = tmpDir </> "test.history"
    history <- initGameHistory logPath histPath
    return $ GameState world history

-- | Run a command in the test environment
runCommand :: CommandExecutor -> Expression -> GameWorld -> IO (Text, GameWorld)
runCommand executor expr initialWorld = do
    state <- initTestState initialWorld
    (result, finalState) <- runStateT (executor expr) state
    return (result, gsWorld finalState)

-- Common test context helpers
verifyStartLocation :: GameWorld -> Text -> Expectation
verifyStartLocation gw expectedLoc = do
    let acLoc = locTag $ getActiveActorLoc gw
    acLoc `shouldBe` expectedLoc