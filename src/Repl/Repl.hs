module Repl.Repl (replLoop) where

import           Control.Monad.State
import           Core.Config         (quitCommands, replPrompt)
import           Core.State          (AppState (..))
import           Data.Text           (Text)
import qualified Data.Text.IO        as TIO
import           Logger
import           Repl.Interpreter    (interpretCommand)
import           System.IO           (hFlush, stdout)

-- | Main REPL loop that handles both game state and logging
replLoop :: AppState -> IO (Maybe AppState)
replLoop appState = do
    input <- read_

    if input `elem` quitCommands
    then do
        finalHistory <- logInfo (gameHistory appState) "Player requested game exit"
        saveHistory finalHistory
        return Nothing
    else do
        latestHistory <- logGameAction (gameHistory appState) input

        let (outputM, newWorld) = runState (interpretCommand input) (gameWorld appState)
        case outputM of
            Just output -> do
                print_ output
                updatedHistory <- logInfo latestHistory $ "Command output: " <> output
                return $ Just $ appState
                    { gameWorld = newWorld
                    , gameHistory = updatedHistory
                    }
            Nothing -> do
                finalHistory <- logInfo latestHistory "Command resulted in game exit"
                saveHistory finalHistory
                return Nothing

read_ :: IO Text
read_ = do
    TIO.putStr replPrompt
    hFlush stdout
    TIO.getLine

print_ :: Text -> IO ()
print_ = TIO.putStrLn
