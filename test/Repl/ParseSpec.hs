module Repl.ParseSpec (spec) where

import           Control.Monad.State
import           Core.State          (GameWorld(..))
import           Data.Text           (Text)
import           Repl.Parse         (Command(Command), tryCommand, parse)
import           Test.Hspec
import           Command.Common     (CommandExecutor)
import           Mock.GameEnvironment
import qualified Data.Text          as T

-- Helper to evaluate State actions
evalCommand :: Maybe (State GameWorld Text) -> Maybe Text
evalCommand Nothing = Nothing
evalCommand (Just stateAction) = Just $ evalState stateAction defaultGW

-- Helper function to run parse in State monad
runParse :: Text -> Maybe Text
runParse input = evalState (parse input) defaultGW

-- Testing for unrecognized commands
shouldBeUnrecognizedCommand :: Text -> Expectation
shouldBeUnrecognizedCommand input =
    case runParse input of
        Just result -> result `shouldBe` ("Don't know how to " <> input <> ".")
        Nothing -> expectationFailure $
            "Expected 'Don't know how to' message but got Nothing for input: " ++ show input

-- Testing for quit commands that should return Nothing
shouldBeQuitCommand :: Text -> Expectation
shouldBeQuitCommand input =
    runParse input `shouldBe` Nothing

spec :: Spec
spec = do
    describe "Command Parser" $ do
        -- context "Pure Parser Behavior" $ do
        --     let dummyLookExecutor :: CommandExecutor
        --         dummyLookExecutor = const $ return "looked"

        --         dummyGoExecutor :: CommandExecutor
        --         dummyGoExecutor = \dir -> return $ "went " <> dir

        --         lookCmd = Command "look" dummyLookExecutor
        --         goCmd = Command "go" dummyGoExecutor

            -- it "matches exact commands" $ do
            --     evalCommand (tryCommand "look" lookCmd) `shouldBe` Just "looked"

            -- it "matches commands with arguments" $ do
            --     evalCommand (tryCommand "go north" goCmd) `shouldBe` Just "went north"

            -- it "handles non-matching commands" $ do
            --     evalCommand (tryCommand "jump" lookCmd) `shouldBe` Nothing
            --     evalCommand (tryCommand "run" goCmd) `shouldBe` Nothing

            -- it "handles case sensitivity" $ do
            --     evalCommand (tryCommand "LOOK" lookCmd) `shouldBe` Just "looked"
            --     evalCommand (tryCommand "Go NoRtH" goCmd) `shouldBe` Just "went NoRtH"

        context "Game Integration" $ do
            it "handles look command" $ do
                runParse "look" `shouldBe` Just "You are in a flowery meadow."
                runParse "look around" `shouldBe`
                    Just "You are in a flowery meadow. You look around and see a simple bag, a shiny bauble, a magic eight ball and a bag of holding."

            it "handles go command with valid destination" $ do
                runParse "go cave" `shouldBe` Just "Moving to cave."

            it "handles go command with invalid destination" $ do
                runParse "go narnia" `shouldBe`
                    Just "There is no indication there's a way to get to narnia."

            it "handles get command with available item" $ do
                runParse "get bauble" `shouldBe`
                    Just "Moved bauble to Alice the Adventurer"

            it "handles get command with unavailable item" $ do
                runParse "get unicorn" `shouldBe`
                    Just "Cannot pick up \"unicorn\"."

            it "handles inventory command" $ do
                runParse "inventory" `shouldBe` Just "Your inventory is: nothing"

        context "Special Commands" $ do
            it "handles empty or whitespace input" $ do
                shouldBeUnrecognizedCommand ""
                shouldBeUnrecognizedCommand "   "

            it "recognizes quit commands" $ do
                shouldBeQuitCommand ":q"
                shouldBeQuitCommand "quit"
                shouldBeQuitCommand "exit"

            it "handles quit commands case-insensitively" $ do
                shouldBeQuitCommand ":Q"
                shouldBeQuitCommand "QUIT"
                shouldBeQuitCommand "Exit"

        context "Put Commands" $ do
            it "handles valid put commands with containers" $ do
                runParse "put bauble in bag" `shouldBe`
                    Just "Don't see a bauble."  -- Since we haven't picked it up yet

            it "handles put commands with non-containers" $ do
                runParse "put bauble in bat" `shouldBe`
                    Just "Don't see a bauble."

            it "handles malformed put commands" $ do
                runParse "put" `shouldBe` Just "Don't know where to put."
                runParse "put ball" `shouldBe` Just "Don't know where to put ball."
                runParse "put ball box" `shouldBe` Just "Don't know where to put ball box."