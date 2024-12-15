{-# LANGUAGE OverloadedStrings #-}

module Repl.InterpreterSpec (spec) where

import           Command.TestUtils
import           Control.Monad.State
import           Core.GameState
import           Data.Text           (isPrefixOf)
import           Mock.TestWorld
import           Repl.Interpreter
import           Test.Hspec

spec :: Spec
spec = describe "Interpreter" $ do
  describe "tryCommand" $ do
    it "succeeds for valid commands" $ do
      initialState <- initTestState defaultGW
      case tryCommand "inventory" of
        Right action -> do
          (output, finalState) <- runStateT action initialState
          -- "Your inventory" `isPrefixOf` output `shouldBe` True
          gsWorld finalState `shouldBe` defaultGW
        Left err -> expectationFailure $ "Expected Right but got Left: " ++ show err

    it "returns Left for malformed commands" $ do
      case tryCommand "" of
        Left err -> err `shouldBe` "Did you mean to type a command?"
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
      case result of
        Just txt -> "I don't understand 'nonexistent'" `isPrefixOf` txt `shouldBe` True
        Nothing  -> expectationFailure "Expected Just but got Nothing"
      gsWorld finalState `shouldBe` defaultGW

    it "returns the command output for known commands" $ do
      initialState <- initTestState defaultGW
      (result, finalState) <- runStateT (interpretCommand "inventory") initialState
      case result of
        Just txt -> "Your inventory" `isPrefixOf` txt `shouldBe` True
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
        Just txt -> "I couldn't understand" `isPrefixOf` txt `shouldBe` True
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
