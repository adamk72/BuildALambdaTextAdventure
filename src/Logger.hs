{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Logger
    ( GameHistory (..)
    , LogEntry (..)
    , LogLevel (..)
    , getRecentCommands
    , initGameHistory
    , loadHistory
    , logCommand
    , logDebug
    , logError
    , logInfo
    , saveHistory
    ) where

import           Control.Exception
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Data.Time              (UTCTime, getCurrentTime)
import           System.Directory       (createDirectoryIfMissing)
import           System.FilePath        (takeDirectory)

data LogLevel = Debug | Info | Error
    deriving (Show, Eq, Ord)

-- | Structure for a single log entry
data LogEntry = LogEntry
    { timestamp :: UTCTime
    , level     :: LogLevel
    , message   :: T.Text
    , isCommand :: Bool  -- Flag to identify if this is a player command
    } deriving (Show, Eq)

-- | Game history containing both logs and recent commands
data GameHistory = GameHistory
    { logEntries        :: [LogEntry]       -- All log entries
    , recentCommands    :: [T.Text]     -- Recent player commands only
    , maxCommandHistory :: Int       -- Maximum number of commands to keep
    , logFile           :: FilePath           -- Path to log file
    , historyFile       :: FilePath       -- Path to command history file
    } deriving (Show)

-- | Initialize a new game history
initGameHistory :: FilePath -> FilePath -> IO GameHistory
initGameHistory logPath histPath = do
    createDirectoryIfMissing True (takeDirectory logPath)
    createDirectoryIfMissing True (takeDirectory histPath)
    return $ GameHistory
        { logEntries = []
        , recentCommands = []
        , maxCommandHistory = 50
        , logFile = logPath
        , historyFile = histPath
        }

-- | Log a player command - adds to command history and logs as Info
logCommand :: MonadIO m => GameHistory -> T.Text -> m GameHistory
logCommand hist cmd = do
    timestamp <- liftIO getCurrentTime
    let entry = LogEntry timestamp Info cmd True
        newHist = hist
            { logEntries = entry : logEntries hist
            , recentCommands = take (maxCommandHistory hist) (cmd : recentCommands hist)
            }
    -- Write to log file
    liftIO $ TIO.appendFile (logFile hist) (formatLogEntry entry)
    return newHist

-- | Log a debug message
logDebug :: MonadIO m => GameHistory -> T.Text -> m GameHistory
logDebug hist = logMessage hist Debug False

-- | Log an info message
logInfo :: MonadIO m => GameHistory -> T.Text -> m GameHistory
logInfo hist = logMessage hist Info False

-- | Log an error message
logError :: MonadIO m => GameHistory -> T.Text -> m GameHistory
logError hist = logMessage hist Error False

-- | Internal helper for logging messages
logMessage :: MonadIO m => GameHistory -> LogLevel -> Bool -> T.Text -> m GameHistory
logMessage hist level isCmd msg = do
    timestamp <- liftIO getCurrentTime
    let entry = LogEntry timestamp level msg isCmd
        newHist = hist { logEntries = entry : logEntries hist }
    -- Write to log file
    liftIO $ TIO.appendFile (logFile hist) (formatLogEntry entry)
    return newHist

-- | Format a log entry for file output
formatLogEntry :: LogEntry -> T.Text
formatLogEntry LogEntry{..} = T.unwords
    [ T.pack (show timestamp)
    , T.pack (show level)
    , if isCommand then "[COMMAND]" else ""
    , message
    , "\n"
    ]

-- | Get recent commands for UI history
getRecentCommands :: GameHistory -> [T.Text]
getRecentCommands = recentCommands

-- | Save game history to file
saveHistory :: GameHistory -> IO ()
saveHistory hist = do
    -- Write all pending log entries
    let logContent = T.unlines $ map formatLogEntry (reverse $ logEntries hist)
    TIO.appendFile (logFile hist) logContent

    -- Write command history
    TIO.writeFile (historyFile hist) $ T.unlines (reverse $ recentCommands hist)

-- | Load game history from file
loadHistory :: FilePath -> FilePath -> IO GameHistory
loadHistory logPath histPath = do
    hist <- initGameHistory logPath histPath
    -- Load previous commands if they exist
    commands <- TIO.readFile histPath `catch` \(_ :: IOError) -> return ""
    let loadedCommands = filter (not . T.null) $ T.lines commands
    return hist { recentCommands = loadedCommands }
