{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Fuse foldr/map" #-}

module Repl.Interpreter (interpretCommand, tryCommand) where

import           Command.CommandInfo
import           Control.Monad.State
import           Core.Config          (quitCommands)
import           Core.State.GameState (GameWorld)
import           Data.Text            (Text, toLower, words)
import           Parser.Parser
import           Prelude              hiding (words)

tryCommand :: CommandInfo -> Text -> Either Text (State GameWorld Text)
tryCommand cmd input =
    case parsePhrase input of
        Left err   -> Left $ renderExpressionError err
        Right expr -> Right $ cmdExec cmd expr

interpretCommand :: Text -> State GameWorld (Maybe Text)
interpretCommand raw = do
  let input = toLower raw
      match = case findCommand (head $ words input) of
        Just cmdInfo -> tryCommand cmdInfo input
        Nothing      -> Left "Unknown command"
  case match of
    Right action -> do
        Just <$> action
    Left err -> do
      if input `elem` quitCommands
      then return Nothing
      else return $ Just $ "Don't know how to " <> input <> ". Got error: " <> err <> "."
