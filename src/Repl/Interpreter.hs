{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Fuse foldr/map" #-}

module Repl.Interpreter (interpretCommand, tryCommand) where

import           Command.CommandInfo
import           Core.Config          (quitCommands)
import           Core.GameMonad
import           Core.State.GameState
import           Data.Text            (Text, toLower, words)
import           Parser.Parser
import           Prelude              hiding (words)

tryCommand :: CommandInfo -> Text -> Either Text (GameMonad Text)
tryCommand cmd input =
    case parsePhrase input of
        Left err   -> Left $ renderExpressionError err
        Right expr -> Right $ cmdExec cmd expr

interpretCommand :: Text -> GameMonad (Maybe Text)
interpretCommand raw = do
    let input = toLower raw
    let match = case findCommand (head $ words input) of
            Just cmdInfo -> tryCommand cmdInfo input
            Nothing      -> Left "Unknown command"
    case match of
        Right action -> do
            logGameInfo $ "Executing command: " <> input
            result <- action
            logGameInfo $ "Command result: " <> result
            return $ Just result
        Left err -> do
            if input `elem` quitCommands
            then do
                logGameInfo "Quit command received"
                return Nothing
            else do
                logGameError $ "Command error: " <> err
                return $ Just $ "Don't know how to " <> input <> ". Got error: " <> err <> "."
