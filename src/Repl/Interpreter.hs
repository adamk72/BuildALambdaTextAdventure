{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Fuse foldr/map" #-}

module Repl.Interpreter (interpretCommand, tryCommand) where

import           Command.CommandDefinition
import           Command.CommandExecutor   (runScenarioCheck)
import           Control.Monad.State       (gets, modify)
import           Core.Config               (quitCommands)

import           Core.GameMonad
import           Core.State.GameState
import           Data.Text                 (Text, toLower)
import qualified Data.Text                 as T
import           Logger                    (logCommand)
import           Parser.Parser
import           Parser.Utils              (getVerb)
import           Prelude                   hiding (words)

tryCommand :: Text -> Either Text (GameMonad Text)
tryCommand input =
  case parseCmdPhrase input of
    Left err -> Left $ renderExpressionError err
    Right expr -> do
      let verb = getVerb expr
      case findCommand verb of
        Just cmdInfo -> case cmdExec cmdInfo of
          Left executor  -> Right $ executor expr
          Right executor -> Right $ runScenarioCheck executor expr
        Nothing -> Left $ "I don't understand '" <> verb <> "'. Valid phrases start with: " <> T.intercalate ", " knownCmdVerbs <> "."

logPlayerCommand :: Text -> GameMonad ()
logPlayerCommand cmd = do
    oldHistory <- gets gsHistory
    newHistory <- logCommand oldHistory cmd
    modify $ \s -> s { gsHistory = newHistory }

interpretCommand :: Text -> GameMonad (Maybe Text)
interpretCommand raw = do
    let input = toLower raw
    case tryCommand input of
        Right action -> do
            logGameInfo $ "Executing command: " <> input
            logPlayerCommand input
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
                return $ Just err
