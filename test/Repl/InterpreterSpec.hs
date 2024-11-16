{-# LANGUAGE OverloadedStrings #-}
module Repl.InterpreterSpec (spec) where

import           Command.CommandExecutor (CommandExecutor)
import           Command.CommandInfo     (CommandInfo (..), CommandVerb (..))
import           Command.TestUtils
import           Control.Monad.State
import           Core.State.GameState
import           Data.Text               (Text, isPrefixOf)
import Mock.TestWorld
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

-- Test suite
spec :: Spec
spec = describe "Interpreter" $ do
    describe "tryCommand" $ do
        let testCmd = mkTestCommand LookVerb "test" [] mockSuccessExecutor

        it "succeeds for valid commands" $ do
            initialState <- initTestState defaultGW
            case tryCommand testCmd "test around" of
                Right action -> do
                    (output, finalState) <- runStateT action initialState
                    output `shouldBe` "Success!"
                    gsWorld finalState `shouldBe` defaultGW
                Left err -> expectationFailure $ "Expected Right but got Left: " ++ show err

        it "returns Left for malformed commands" $ do
            case tryCommand testCmd "" of
                Left err -> err `shouldBe` "I couldn't understand ''. Please try rephrasing your command."
                Right _  -> expectationFailure "Expected Left but got Right"

    describe "interpretCommand" $ do
        it "handles quit commands by returning Nothing" $ do
            initialState <- initTestState defaultGW
            (result, finalState) <- runStateT (interpretCommand ":quit") initialState
            result `shouldBe` Nothing
            gsWorld finalState `shouldBe` defaultGW

        it "handles unknown commands with an error message" $ do
            initialState <- initTestState defaultGW
            (result, finalState) <- runStateT (interpretCommand "nonexistent") initialState
            result `shouldBe` Just "Don't know how to nonexistent. Got error: Unknown command."
            gsWorld finalState `shouldBe` defaultGW

        it "returns the command output for known commands" $ do
            initialState <- initTestState defaultGW
            (result, finalState) <- runStateT (interpretCommand "inventory") initialState
            case result of
                Just txt -> "Your inventory is: " `isPrefixOf` txt `shouldBe` True
                Nothing  -> expectationFailure "Expected Just but got Nothing"
            gsWorld finalState `shouldBe` defaultGW

        it "is case insensitive" $ do
            initialState <- initTestState defaultGW
            (result1, state1) <- runStateT (interpretCommand "INVENTORY") initialState
            (result2, state2) <- runStateT (interpretCommand "inventory") initialState
            result1 `shouldBe` result2
            gsWorld state1 `shouldBe` gsWorld state2
            gsWorld state1 `shouldBe` defaultGW

        it "handles invalid command phrases" $ do
            initialState <- initTestState defaultGW
            (result, finalState) <- runStateT (interpretCommand "look !!!") initialState
            case result of
                Just txt -> "Don't know how to look !!!." `isPrefixOf` txt `shouldBe` True
                -- Just txt -> "Don't know how to look !!!."  `shouldBe` txt
                Nothing  -> expectationFailure "Expected Just but got Nothing"
            gsWorld finalState `shouldBe` defaultGW

        it "preserves game state for unknown commands" $ do
            initialState <- initTestState defaultGW
            (_, finalState) <- runStateT (interpretCommand "nonsense") initialState
            gsWorld finalState `shouldBe` defaultGW

        it "preserves game state for quit commands" $ do
            initialState <- initTestState defaultGW
            (_, finalState) <- runStateT (interpretCommand ":q") initialState
            gsWorld finalState `shouldBe` defaultGW
