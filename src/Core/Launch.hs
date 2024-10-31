{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use forM_" #-}

module Core.Launch (launch) where

import           Core.State (GameEnvironment (world), GameWorld,
                             loadGameEnvironmentJSON)
import           Repl.Repl  (loop)

gameLoop :: GameWorld -> IO ()
gameLoop initialState = do
    nextState <- loop initialState
    case nextState of
        Just newState -> gameLoop newState
        Nothing       -> return ()
    -- Data.Foldable.forM_ nextState gameLoop -- Advanced version, based on LSP suggestion.

launch :: FilePath -> IO ()
launch fp = loadGameEnvironmentJSON fp >>= either print (startGame . world) -- `either` comes in handy here.
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