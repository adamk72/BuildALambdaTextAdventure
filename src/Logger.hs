{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fromRight" #-}
module Logger
    ( GameHistory (..)
    , LogEntry (..)
    , LogLevel (..)
    , getRecentCommands
    , initGameHistory
    , loadHistory
    , logDebug
    , logError
    , logGameAction
    , logInfo
    , saveHistory
    ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson             ((.:), (.=))
import qualified Data.Aeson             as A
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Data.Time              (UTCTime, getCurrentTime)
import           GHC.Generics           (Generic)
import           System.Directory       (createDirectoryIfMissing)
import           System.FilePath        (takeDirectory)

-- | Log levels for different types of messages
data LogLevel = Debug | Info | Error
    deriving (Show, Eq, Ord, Read, Generic)

-- | Custom JSON instances for LogLevel
instance A.ToJSON LogLevel where
    toJSON Debug = A.String "Debug"
    toJSON Info  = A.String "Info"
    toJSON Error = A.String "Error"

instance A.FromJSON LogLevel where
    parseJSON = A.withText "LogLevel" $ \t -> case t of
        "Debug" -> pure Debug
        "Info"  -> pure Info
        "Error" -> pure Error
        _       -> fail $ "Invalid LogLevel: " ++ show t

-- | Structure for a single log entry
data LogEntry = LogEntry
    { timestamp :: UTCTime
    , level     :: LogLevel
    , message   :: T.Text
    , isCommand :: Bool  -- Indicates if this entry is a player command
    } deriving (Show, Eq, Generic)

-- | Game history containing both logs and recent commands
data GameHistory = GameHistory
    { logEntries        :: [LogEntry]       -- All log entries
    , recentCommands    :: [T.Text]     -- Recent player commands only
    , maxCommandHistory :: Int       -- Maximum number of commands to keep
    , logFile           :: FilePath           -- Path to log file
    , historyFile       :: FilePath       -- Path to command history file
    } deriving (Show)

-- JSON instances for serialization
instance A.ToJSON LogEntry where
    toJSON entry = A.object
        [ "timestamp" .= timestamp entry
        , "level" .= level entry
        , "message" .= message entry
        , "isCommand" .= isCommand entry
        ]

instance A.FromJSON LogEntry where
    parseJSON = A.withObject "LogEntry" $ \v -> LogEntry
        <$> v .: "timestamp"
        <*> v .: "level"
        <*> v .: "message"
        <*> v .: "isCommand"

-- | Initialize a new game history with default settings
initGameHistory :: FilePath -> FilePath -> IO GameHistory
initGameHistory logPath histPath = do
    -- Create directories if they don't exist
    createDirectoryIfMissing True (takeDirectory logPath)
    createDirectoryIfMissing True (takeDirectory histPath)
    return $ GameHistory
        { logEntries = []
        , recentCommands = []
        , maxCommandHistory = 50  -- Store last 50 commands
        , logFile = logPath
        , historyFile = histPath
        }

-- | Log a game action (command) with automatic command history update
logGameAction :: MonadIO m => GameHistory -> T.Text -> m GameHistory
logGameAction hist cmd = do
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
logDebug hist = logMessage hist Debug

-- | Log an info message
logInfo :: MonadIO m => GameHistory -> T.Text -> m GameHistory
logInfo hist = logMessage hist Info

-- | Log an error message
logError :: MonadIO m => GameHistory -> T.Text -> m GameHistory
logError hist = logMessage hist Error

-- | Helper function to log messages of any level
logMessage :: MonadIO m => GameHistory -> LogLevel -> T.Text -> m GameHistory
logMessage hist level msg = do
    timestamp <- liftIO getCurrentTime
    let entry = LogEntry timestamp level msg False
        newHist = hist { logEntries = entry : logEntries hist }
    -- Write to log file
    liftIO $ TIO.appendFile (logFile hist) (formatLogEntry entry)
    return newHist

-- | Format a log entry for file output
formatLogEntry :: LogEntry -> T.Text
formatLogEntry LogEntry{..} = T.unwords
    [ T.pack (show timestamp)
    , T.pack (show level)
    , message
    , T.pack (if isCommand then "[command]" else "")
    , "\n"
    ]

-- | Get recent commands for UI history
getRecentCommands :: GameHistory -> [T.Text]
getRecentCommands = recentCommands

-- | Save game history to file
saveHistory :: GameHistory -> IO ()
saveHistory hist = do
    -- Save command history
    A.encodeFile (historyFile hist) (recentCommands hist)
    -- Log entries are already saved incrementally

-- | Load game history from file
loadHistory :: FilePath -> FilePath -> IO GameHistory
loadHistory logPath histPath = do
    hist <- initGameHistory logPath histPath
    -- Load command history
    commands <- either (const []) id <$> A.eitherDecodeFileStrict' histPath
    return hist { recentCommands = commands }
