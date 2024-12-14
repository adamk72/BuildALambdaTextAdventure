module Command.Replay (ReplayMessage(..), executeReplay) where

import           Command.Message.Common     (MessageRenderer(..))
import           Core.GameMonad            (GameMonad, logGameError, logGameInfo)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Repl.Replay               (ReplayResult(..), executeReplayFile)
import           Repl.Interpreter          (interpretCommand)

data ReplayMessage
    = ReplayFileNotFound Text
    | ReplayStarting Text
    | ReplayComplete Text Int
    | ReplayError Text Text
    deriving (Show, Eq)

instance MessageRenderer ReplayMessage where
    renderMessage = \case
        ReplayFileNotFound file -> "Could not find replay file: " <> file
        ReplayStarting file -> "Starting replay from file: " <> file
        ReplayComplete file count -> "Completed replay from " <> file <> ". Executed " <> T.pack (show count) <> " commands."
        ReplayError file err -> "Error during replay of " <> file <> ": " <> err

executeReplay :: Text -> GameMonad Text
executeReplay filename = do
    logGameInfo $ "Starting replay from: " <> filename
    result <- executeReplayFile filename interpretCommand
    case result of
        FileNotFound _ -> do
            logGameError $ "Replay file not found: " <> filename
            return $ renderMessage $ ReplayFileNotFound filename
        ReplayFailure err -> do
            logGameError $ "Replay error: " <> err
            return $ renderMessage $ ReplayError filename err
        ReplaySuccess count -> do
            logGameInfo $ "Replay completed successfully from: " <> filename
            return $ renderMessage $ ReplayComplete filename count