module Repl.Repl (replLoop) where

import           Control.Monad.State (runStateT)
import           Core.Config         (quitCommands, replPrompt)
import           Core.GameMonad
import           Core.State          (AppState (..), GameState (..))
import           Data.Text           (Text)
import qualified Data.Text.IO        as TIO
import           Logger
import           Repl.Interpreter    (interpretCommand)
import           System.IO           (hFlush, stdout)

replLoop :: AppState -> IO (Maybe AppState)
replLoop AppState{gameWorld = world, gameHistory = history} = do
    input <- read_

    if input `elem` quitCommands
    then do
        finalHistory <- logInfo history "Player requested game exit"
        saveHistory finalHistory
        return Nothing
    else do
        let initialState = GameState world history
        (result, GameState newWorld newHistory) <-
            runStateT (interpretCommand input) initialState

        case result of
            Just output -> do
                print_ output
                newHistory2 <- logInfo newHistory "Command completed"
                return $ Just $ AppState newWorld newHistory2
            Nothing -> do
                finalHistory <- logInfo newHistory "Game exit requested"
                saveHistory finalHistory
                return Nothing

read_ :: IO Text
read_ = do
    TIO.putStr replPrompt
    hFlush stdout
    TIO.getLine

print_ :: Text -> IO ()
print_ = TIO.putStrLn
