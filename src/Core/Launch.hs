{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use forM_" #-}

module Core.Launch (launch) where

import           Core.State             (AppState (..), loadGameEnvironmentJSON)
import           Core.State.GameState   (World)
import           Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Entity.EntityConverter (convertToEntityWorld)
import           Logger
import           Repl.Repl              (replLoop)
import           System.FilePath        (takeDirectory, (</>))

logFileName :: String
logFileName = "game.log"

cmdHistoryFileName :: String
cmdHistoryFileName = "cmd_history.log"

initAppState :: World -> FilePath -> Bool -> IO AppState
initAppState gw baseDir replayMode = do
    let logPath = baseDir </> "logs" </> logFileName
        histPath = baseDir </> "logs" </> cmdHistoryFileName

    history <- initGameHistory logPath histPath
    newHistory <- logInfo history "Initializing new game state"

    commands <- if replayMode
               then do
                   -- Todo #1: handle file failures
                   -- Todo #2: handle other filename
                   contents <- TIO.readFile (historyFile history)
                   return $ Prelude.filter (not . T.null) $ T.lines contents
               else return []

    return $ AppState
        { gameWorld = gw
        , gameHistoryLog = newHistory
        , isReplayMode = replayMode
        , replayCommands = commands
        }

gameLoop :: AppState -> IO (Either Text ())
gameLoop state = do
    newHistory <- logDebug (gameHistoryLog state) "Starting new game loop iteration"
    let updatedState = state { gameHistoryLog = newHistory }

    nextState <- replLoop updatedState
    case nextState of
        Just newState -> gameLoop newState
        Nothing -> do
            finalHistory <- logInfo (gameHistoryLog state) "Game ended normally"
            saveHistory finalHistory
            return (Right ())

launch :: FilePath -> Bool -> IO (Either Text ())
launch fp replayMode = do
    fileResult <- loadGameEnvironmentJSON fp
    case fileResult of
        Left err -> do
                let logPath = takeDirectory fp </> "logs" </> logFileName
                    histPath = takeDirectory fp </> "logs" </> cmdHistoryFileName
                history <- initGameHistory logPath histPath
                _ <- logError history $ "Error loading game: " <> pack (show err)
                return $ Left $ "Error loading game: " <> pack (show err)

        Right (_, world) ->
            case world of
                Nothing -> do
                    let logPath = takeDirectory fp </> "logs" </> logFileName
                        histPath = takeDirectory fp </> "logs" </> cmdHistoryFileName
                    history <- initGameHistory logPath histPath
                    _ <- logError history "No game world found in environment"
                    return $ Left "No game world found!"

                Just gwJSON -> do
                    let gwE = convertToEntityWorld gwJSON
                    case gwE of
                        Right gw -> do
                            state <- initAppState gw (takeDirectory fp) replayMode
                            newHistory <- logInfo (gameHistoryLog state) $
                                "Game loaded successfully from: " <> pack fp
                            gameLoop state { gameHistoryLog = newHistory }
                        Left err -> return $ Left ("Something went wrong with parsing the JSON: " <> err)
