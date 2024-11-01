{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use forM_" #-}

module Core.Launch (launch) where

import           Core.State (GameEnvironment (world), GameWorld,
                             loadGameEnvironmentJSON)
import           Data.Text  (Text, pack)
import           Repl.Repl  (loop)

gameLoop :: Maybe GameWorld -> IO (Either Text ())
gameLoop Nothing = return (Left "No game world found!")
gameLoop (Just initialState) = do
    nextState <- loop initialState
    case nextState of
        Just newState -> gameLoop (Just newState)
        Nothing       -> return (Right ())
    -- Data.Foldable.forM_ nextState gameLoop -- Advanced version, based on LSP suggestion.

launch :: FilePath -> IO (Either Text ())
launch fp = do
    result <- loadGameEnvironmentJSON fp
    case result of
        Left err -> return (Left $ "Error loading game: " <> pack (show err))
        Right gameEnv -> startGame (world gameEnv)
  where
    startGame w = print w >> gameLoop w

-- Todo: Note this as a trigger pattern
{- Previous version for comparison
launch :: FilePath -> IO ()
launch fp = do
   geJSON <- loadGameEnvironmentJSON fp
   case geJSON of
    Right adventure -> do
        print $ world adventure
        gameLoop $ world adventure
    Left e -> print e
-}