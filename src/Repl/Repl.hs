module Repl.Repl (replLoop) where

import           Control.Monad.State (runStateT)
import           Core.Config         (replPrompt)
import           Core.State          (AppState (..), GameState (..))
import           Data.Text           (Text)
import qualified Data.Text.IO        as TIO
import           Logger
import           Repl.Interpreter    (interpretCommand)
import           System.IO           (hFlush, stdout)

replLoop :: AppState -> IO (Maybe AppState)
replLoop state@AppState{gameWorld = world, gameHistoryLog = history, isReplayMode = replay, replayCommands = commands} = do
    input <- read_ state
    case input of
        Nothing -> return Nothing
        Just cmd -> do
            let initialState = GameState world history
            (result, GameState newWorld newHistory) <- runStateT (interpretCommand cmd) initialState
            case result of
                Just output -> do
                    print_ output
                    updatedHistory <- logInfo newHistory "Command completed"
                    return $ Just $ AppState
                        { gameWorld = newWorld
                        , gameHistoryLog = updatedHistory
                        , isReplayMode = replay
                        , replayCommands = drop 1 commands
                        }
                Nothing -> do
                    finalizedHistory <- logInfo newHistory "Game exit requested"
                    saveHistory finalizedHistory
                    return Nothing

read_ :: AppState -> IO (Maybe Text)
read_ AppState{isReplayMode = False} = do
    TIO.putStr replPrompt
    hFlush stdout
    Just <$> TIO.getLine
read_ AppState{isReplayMode = True, replayCommands = []} =
    return Nothing
read_ AppState{isReplayMode = True, replayCommands = (cmd:_)} = do
    TIO.putStr replPrompt
    TIO.putStrLn cmd
    hFlush stdout
    return $ Just cmd

print_ :: Text -> IO ()
print_ = TIO.putStrLn
