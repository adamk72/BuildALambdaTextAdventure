{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Fuse foldr/map" #-}

module Repl.Interpreter (interpretCommand, tryCommand, Command(Command), firstRight) where

import           Command.Commands
import           Command.Definitions
import           Control.Monad.State
import           Core.Config          (quitCommands)
import           Core.State.GameState (GameWorld)
import           Data.Text            (Text, toLower)
import           Parser.Parser

firstRight :: Either Text a -> Either Text a -> Either Text a
firstRight (Right x) _        = Right x
firstRight (Left _) (Right y) = Right y
firstRight (Left x) (Left _)  = Left x  -- Keep the first error we found

data Command = Command
  { cmdName    :: Text
  , cmdExecute :: CommandExecutor
  }

toCommand :: CommandInfo -> Command
toCommand info = Command (cmdText info) (cmdExec info)

commands :: [Command]
commands = map toCommand allCommands

tryCommand :: Text -> Command -> Either Text (State GameWorld Text)
tryCommand input cmd =
    case parsePhrase input of
        Left err   -> Left $ renderExpressionError err
        Right expr -> Right $ cmdExecute cmd expr

interpretCommand :: Text -> State GameWorld (Maybe Text)
interpretCommand input = do
  let lower = toLower input
      match = foldr firstRight (Left "No matching command found") $ map (tryCommand lower) commands
  case match of
    Right action -> do
        Just <$> action
    Left err -> do
      if lower `elem` quitCommands
      then return Nothing
      else return $ Just $ "Don't know how to " <> lower <> ". Got error: " <> err <> "."
