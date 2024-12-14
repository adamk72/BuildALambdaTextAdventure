module Repl.Replay (ReplayResult(..), replayCommandsFromFile) where

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Core.GameMonad            (GameMonad, logGameError, logGameInfo)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           System.Directory          (doesFileExist)

data ReplayResult
    = ReplaySuccess Int
    | ReplayFailure Text
    | FileNotFound Text
    deriving (Show, Eq)

replayCommandsFromFile :: (MonadIO m) => Text -> (Text -> m (Maybe Text)) -> m ReplayResult
replayCommandsFromFile filename commandHandler = do
    exists <- liftIO $ doesFileExist (T.unpack filename)
    if not exists
        then return $ FileNotFound filename
        else do
            content <- liftIO $ TIO.readFile (T.unpack filename)
            let commands = filter (not . T.null) $ T.lines content
            executeCommands commands 0
  where
    executeCommands [] count =
        return $ ReplaySuccess count
    executeCommands (cmd:rest) count = do
        result <- commandHandler cmd
        case result of
            Just _ -> executeCommands rest (count + 1)
            Nothing -> return $ ReplayFailure $ "Command failed: " <> cmd