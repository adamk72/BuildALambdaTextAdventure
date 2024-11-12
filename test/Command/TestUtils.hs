{-# LANGUAGE OverloadedStrings #-}
module Command.TestUtils (module Command.TestUtils) where

import           Command.CommandExecutor
import           Control.Monad.State
import           Core.State
import           Data.Text              (Text)
import           Logger                 (initGameHistory)
import           Parser.Types           (Expression)
import           System.Directory       (getTemporaryDirectory)
import           System.FilePath        ((</>))
import           Test.Hspec

-- | Create a test GameState with temporary log files
initTestState :: World -> IO GameState
initTestState wrld = do
    tmpDir <- getTemporaryDirectory
    let logPath = tmpDir </> "test.log"
        histPath = tmpDir </> "test.history"
    history <- initGameHistory logPath histPath
    return $ GameState wrld history

-- | Run a command in the test environment
runCommand :: CommandExecutor -> Expression -> World -> IO (Text, World)
runCommand executor expr initialWorld = do
    st <- initTestState initialWorld
    (result, finalState) <- runStateT (executor expr) st
    return (result, gsWorld finalState)

-- Common test context helpers
verifyStartLocation :: World -> Text -> Expectation
verifyStartLocation gw expectedLoc = do
    let acLoc = locTag $ getActiveActorLoc gw
    acLoc `shouldBe` expectedLoc