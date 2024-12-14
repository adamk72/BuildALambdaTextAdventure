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
replLoop AppState{gameWorld = world, gameHistory = history} = do
    input <- read_

    let initialState = GameState world history
    (result, GameState newWorld newHistory) <- runStateT (interpretCommand input) initialState
    case result of
        Just output -> do
            print_ output
            updatedHistory <- logInfo newHistory "Command completed"
            return $ Just $ AppState newWorld updatedHistory
        Nothing -> do
            finalizedHistory <- logInfo newHistory "Game exit requested"
            saveHistory finalizedHistory
            return Nothing

read_ :: IO Text
read_ = do
    TIO.putStr replPrompt
    hFlush stdout
    TIO.getLine

print_ :: Text -> IO ()
print_ = TIO.putStrLn
