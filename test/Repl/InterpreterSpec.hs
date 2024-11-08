{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Repl.InterpreterSpec (spec) where

import Test.Hspec
import Control.Monad.State
import Data.Text (Text, isInfixOf)
import Core.State
import Mock.GameEnvironment
import Repl.Interpreter
import Command.Common (CommandExecutor)
import Parser.Parser

-- Helper function to create a test command
mockCommand :: Text -> CommandExecutor -> Command
mockCommand name executor = Command name executor

-- Mock command executor that just returns its input as text
mockExecutor :: CommandExecutor
mockExecutor expr = return $ "Executed: " <> renderExpression expr

-- Helper to run interpreter commands in a test context
runInterpreter :: Text -> GameWorld -> (Maybe Text, GameWorld)
runInterpreter input = runState (interpretCommand input)

spec :: Spec
spec = do
    describe "firstRight" $ do
        it "returns first Right value when both are Right" $ do
            let result = firstRight (Right "first" :: Either Text Text) (Right "second" :: Either Text Text)
            result `shouldBe` (Right "first" :: Either Text Text)

        it "returns second Right value when first is Left" $ do
            let result = firstRight (Left "error" :: Either Text Text) (Right "success" :: Either Text Text)
            result `shouldBe` (Right "success" :: Either Text Text)

        it "returns first Left value when both are Left" $ do
            let result = firstRight (Left "error1" :: Either Text Text) (Left "error2" :: Either Text Text)
            result `shouldBe` (Left "error1" :: Either Text Text)

    describe "tryCommand" $ do
        let testCommand = mockCommand "test" mockExecutor

        it "returns Right for valid commands" $ do
            case tryCommand "look around" testCommand of
                Right _ -> True `shouldBe` True  -- Command parsed successfully
                Left _ -> expectationFailure "Expected Right, got Left"

        it "returns Left for invalid command syntax" $ do
            case tryCommand "" testCommand of
                Left _ -> True `shouldBe` True   -- Command parsing failed as expected
                Right _ -> expectationFailure "Expected Left, got Right"

    describe "interpretCommand" $ do
        let initialWorld = defaultGW

        it "handles valid 'look' command" $ do
            let (result, _) = runInterpreter "look" initialWorld
            result `shouldSatisfy` \case
                Just txt -> "You are in" `isInfixOf` txt
                Nothing -> False

        it "handles valid 'look around' command" $ do
            let (result, _) = runInterpreter "look around" initialWorld
            result `shouldSatisfy` \case
                Just txt -> "You look around" `isInfixOf` txt
                Nothing -> False

        it "returns Nothing for quit commands" $ do
            let (result, _) = runInterpreter ":quit" initialWorld
            result `shouldBe` Nothing

        it "returns error message for unknown commands" $ do
            let (result, _) = runInterpreter "dance" initialWorld
            result `shouldSatisfy` \case
                Just txt -> "Don't know how to dance" `isInfixOf` txt
                Nothing -> False

        it "maintains game state after command execution" $ do
            let (_, newWorld) = runInterpreter "look" initialWorld
            newWorld `shouldBe` initialWorld  -- 'look' shouldn't modify state

        it "processes commands case-insensitively" $ do
            let (resultLower, _) = runInterpreter "look" initialWorld
            let (resultUpper, _) = runInterpreter "LOOK" initialWorld
            resultLower `shouldBe` resultUpper

    describe "Command integration" $ do
        let initialWorld = defaultGW

        it "can execute movement commands" $ do
            let (result, _) = runInterpreter "go cave" initialWorld
            result `shouldSatisfy` \case
                Just txt -> "Moving to cave" `isInfixOf` txt
                Nothing -> False

        it "can execute inventory commands" $ do
            let (result, _) = runInterpreter "inventory" initialWorld
            result `shouldSatisfy` \case
                Just txt -> "Your inventory is" `isInfixOf` txt
                Nothing -> False