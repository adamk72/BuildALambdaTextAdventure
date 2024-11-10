{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use forM_" #-}

module Core.Launch (launch) where

import           Core.State             (GameEnvironment (world), GameWorld, AppState(..), loadGameEnvironmentJSON)
import           Data.Text              (Text, pack)
import           Logger
import           Repl.Repl              (replLoop)
import           System.FilePath        (takeDirectory, (</>))

initAppState :: GameWorld -> FilePath -> IO AppState
initAppState gw baseDir = do
    let logPath = baseDir </> "logs" </> "game.log"
        histPath = baseDir </> "logs" </> "history.json"

    history <- initGameHistory logPath histPath
    newHistory <- logInfo history "Initializing new game state"

    return $ AppState
        { gameWorld = gw
        , gameHistory = newHistory
        }

gameLoop :: AppState -> IO (Either Text ())
gameLoop state = do
    newHistory <- logDebug (gameHistory state) "Starting new game loop iteration"
    let updatedState = state { gameHistory = newHistory }

    nextState <- replLoop updatedState
    case nextState of
        Just newState -> gameLoop newState
        Nothing -> do
            -- Log game end and save history
            finalHistory <- logInfo (gameHistory state) "Game ended normally"
            saveHistory finalHistory
            return (Right ())


launch :: FilePath -> IO (Either Text ())
launch fp = do
    fileResult <- loadGameEnvironmentJSON fp
    case fileResult of
        Left err -> do
                let logPath = takeDirectory fp </> "logs" </> "game.log"
                    histPath = takeDirectory fp </> "logs" </> "history.json"
                history <- initGameHistory logPath histPath
                _ <- logError history $ "Error loading game: " <> pack (show err)
                return $ Left $ "Error loading game: " <> pack (show err)

        Right gameEnv ->
            case world gameEnv of
                Nothing -> do
                    let logPath = takeDirectory fp </> "logs" </> "game.log"
                        histPath = takeDirectory fp </> "logs" </> "history.json"
                    history <- initGameHistory logPath histPath
                    _ <- logError history "No game world found in environment"
                    return $ Left "No game world found!"

                Just gw -> do
                    state <- initAppState gw (takeDirectory fp)
                    newHistory <- logInfo (gameHistory state) $
                        "Game loaded successfully from: " <> pack fp
                    gameLoop state { gameHistory = newHistory }
