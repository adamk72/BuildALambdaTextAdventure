module Repl.Interpreter (interpretCommand, tryCommand) where

import           Command.Executor
import           Command.Types
import           Control.Monad.State
import           Core.CommandRegistry
import           Core.GameMonad
import           Core.State
import           Data.Text            as T (Text, intercalate, toLower)
import           Logger
import           Parser.Parser        (parseCmdPhrase, renderExpressionError)
import           Parser.Utils

tryCommand :: Text -> Either Text (GameMonad CommandResult)
tryCommand input =
  case parseCmdPhrase input of
    Left err -> Left $ renderExpressionError err
    Right expr -> do
        let verb = getVerb expr
        case findCommand verb allCommands of
            Just cmdInfo -> case cmdHandler cmdInfo of
                QuitCommand                                 -> Right $ return Quit
                BasicCommand cmd                            -> Right $ Continue <$> cmd expr
                ScenarioCommand (ScenarioCheckExecutor cmd) -> Right $ Continue <$> cmd expr
            Nothing -> Left $ "I don't understand '" <> verb <> "'. Valid phrases start with: " <> T.intercalate ", " (getKnownVerbs allCommands) <> "."

logPlayerCommand :: Text -> GameMonad ()
logPlayerCommand cmd = do
    oldHistory <- gets gsHistoryLog
    newHistory <- logCommand oldHistory cmd
    modify $ \s -> s { gsHistoryLog = newHistory }

interpretCommand :: Text -> GameMonad (Maybe Text)
interpretCommand raw = do
    let input = toLower raw
    case tryCommand input of
        Right action -> do
            logGameInfo $ "Executing command: " <> input
            logPlayerCommand input
            result <- action
            case result of
                Continue output -> do
                    logGameInfo $ "Command result: " <> output
                    return $ Just output
                Quit -> do
                    logGameInfo "Quit command received"
                    return Nothing
        Left err -> do
            logGameError $ "Command error: " <> err
            return $ Just err
