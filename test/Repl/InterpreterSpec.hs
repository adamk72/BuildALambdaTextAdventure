{-# LANGUAGE OverloadedStrings #-}
module Repl.InterpreterSpec (spec) where

import           Command.CommandExecutor (CommandExecutor)
import           Command.CommandInfo     (CommandInfo (..), CommandVerb (..))
import           Control.Monad.State
import           Core.State              (GameWorld)
import           Data.Text               (Text, isPrefixOf)
import           Mock.GameEnvironment
import           Repl.Interpreter
import           Test.Hspec

-- Helper function to create test commands
mkTestCommand :: CommandVerb -> Text -> [Text] -> CommandExecutor -> CommandInfo
mkTestCommand verb txt aliases exec = CommandInfo
    { cmdVerb = verb
    , cmdText = txt
    , cmdAliases = aliases
    , cmdExec = exec
    }

-- Mock command executors for testing
mockSuccessExecutor :: CommandExecutor
mockSuccessExecutor _ = return "Success!"

mockFailExecutor :: CommandExecutor
mockFailExecutor _ = return "Failed!"

-- Test suite
spec :: Spec
spec = describe "Interpreter" $ do
    describe "tryCommand" $ do
        let testCmd = mkTestCommand LookVerb "test" [] mockSuccessExecutor

        it "succeeds for valid commands" $ do
            case tryCommand testCmd "test around" of
                Right action -> do
                    let (output, finalState) = runState action defaultGW
                    output `shouldBe` "Success!"
                    finalState `shouldBe` defaultGW
                Left err -> expectationFailure $ "Expected Right but got Left: " ++ show err

        it "returns Left for malformed commands" $ do
            case tryCommand testCmd "" of
                Left err -> err `shouldBe` "I couldn't understand ''. Please try rephrasing your command."
                Right _ -> expectationFailure "Expected Left but got Right"

    describe "interpretCommand" $ do
        it "handles quit commands by returning Nothing" $ do
            let (result, finalState) = runState (interpretCommand ":quit") defaultGW
            result `shouldBe` Nothing
            finalState `shouldBe` defaultGW

        it "handles unknown commands with an error message" $ do
            let (result, finalState) = runState (interpretCommand "nonexistent") defaultGW
            result `shouldBe` Just "Don't know how to nonexistent. Got error: Unknown command."
            finalState `shouldBe` defaultGW

        it "returns the command output for known commands" $ do
            let (result, finalState) = runState (interpretCommand "inventory") defaultGW
            case result of
                Just txt -> "Your inventory is: " `isPrefixOf` txt `shouldBe` True
                Nothing -> expectationFailure "Expected Just but got Nothing"
            finalState `shouldBe` defaultGW

        it "is case insensitive" $ do
            let (result1, state1) = runState (interpretCommand "INVENTORY") defaultGW
            let (result2, state2) = runState (interpretCommand "inventory") defaultGW
            result1 `shouldBe` result2
            state1 `shouldBe` state2
            state1 `shouldBe` defaultGW

        it "handles invalid command phrases" $ do
            let (result, finalState) = runState (interpretCommand "look !!!") defaultGW
            case result of
                Just txt -> "Don't know how to look !!!." `isPrefixOf` txt `shouldBe` True
                Nothing -> expectationFailure "Expected Just but got Nothing"
            finalState `shouldBe` defaultGW

        it "preserves game state for unknown commands" $ do
            let (_, finalState) = runState (interpretCommand "nonsense") defaultGW
            finalState `shouldBe` defaultGW

        it "preserves game state for quit commands" $ do
            let (_, finalState) = runState (interpretCommand ":q") defaultGW
            finalState `shouldBe` defaultGW
