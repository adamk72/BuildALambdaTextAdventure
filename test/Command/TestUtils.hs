{-# LANGUAGE OverloadedStrings #-}

module Command.TestUtils (module Command.TestUtils) where

import           Command.Executor
import           Control.Monad.State
import           Core.State
import           Data.Text                (Text, isInfixOf)
import           Entity.Class.CanMoveSelf
import           Entity.Types.Common
import           Logger                   (initGameHistory)
import           Parser.Types             (CmdExpression)
import           System.Directory         (getTemporaryDirectory)
import           System.FilePath          ((</>))
import           Test.Hspec

initTestState :: World -> IO GameState
initTestState wrld = do
  tmpDir <- getTemporaryDirectory
  let logPath = tmpDir </> "test.log"
      histPath = tmpDir </> "test.history"
  history <- initGameHistory logPath histPath
  return $ GameState wrld history

runCommand :: BasicCommandExecutor -> CmdExpression -> World -> IO (Text, World)
runCommand executor expr initialWorld = do
  st <- initTestState initialWorld
  (result, finalState) <- runStateT (executor expr) st
  return (result, gsWorld finalState)

verifyStartLocation :: World -> Text -> Expectation
verifyStartLocation world expectedLoc =
  getLocationId (activeActor world) `shouldBe` EntityId expectedLoc

outputShouldContain :: Text -> Text -> Expectation
outputShouldContain output expected =
  output `shouldSatisfy` (\t -> expected `Data.Text.isInfixOf` t)
