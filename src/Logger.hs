{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Logger
    ( GameHistoryLog (..)
    , LogEntry (..)
    , LogLevel (..)
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

data LogEntry = LogEntry
    { timestamp :: UTCTime
    , level     :: LogLevel
    , message   :: T.Text
    , isCommand :: Bool
    } deriving (Show, Eq)

data GameHistoryLog = GameHistoryLog
    { logEntries        :: [LogEntry]
    , recentCommands    :: [T.Text]
    , maxCommandHistory :: Int
    , logFile           :: FilePath
    , historyFile       :: FilePath
    } deriving (Show)

initGameHistory :: FilePath -> FilePath -> IO GameHistoryLog
initGameHistory logPath histPath = do
    createDirectoryIfMissing True (takeDirectory logPath)
    createDirectoryIfMissing True (takeDirectory histPath)
    return $ GameHistoryLog
        { logEntries = []
        , recentCommands = []
        , maxCommandHistory = 50
        , logFile = logPath
        , historyFile = histPath
        }

logCommand :: MonadIO m => GameHistoryLog -> T.Text -> m GameHistoryLog
logCommand hist cmd = do
    timestamp <- liftIO getCurrentTime
    let entry = LogEntry timestamp Info cmd True
        newHist = hist
            { logEntries = entry : logEntries hist
            , recentCommands = take (maxCommandHistory hist) (cmd : recentCommands hist)
            }
    liftIO $ TIO.appendFile (logFile hist) (formatLogEntry entry)
    return newHist

logDebug :: MonadIO m => GameHistoryLog -> T.Text -> m GameHistoryLog
logDebug hist = logMessage hist Debug False

logInfo :: MonadIO m => GameHistoryLog -> T.Text -> m GameHistoryLog
logInfo hist = logMessage hist Info False

logError :: MonadIO m => GameHistoryLog -> T.Text -> m GameHistoryLog
logError hist = logMessage hist Error False

logMessage :: MonadIO m => GameHistoryLog -> LogLevel -> Bool -> T.Text -> m GameHistoryLog
logMessage hist level isCmd msg = do
    timestamp <- liftIO getCurrentTime
    let entry = LogEntry timestamp level msg isCmd
        newHist = hist { logEntries = entry : logEntries hist }
    liftIO $ TIO.appendFile (logFile hist) (formatLogEntry entry)
    return newHist

formatLogEntry :: LogEntry -> T.Text
formatLogEntry LogEntry{..} = T.unwords
    [ T.pack (show timestamp)
    , T.pack (show level)
    , if isCommand then "[COMMAND]" else ""
    , message
    , "\n"
    ]

saveHistory :: GameHistoryLog -> IO ()
saveHistory hist = do
    let logContent = T.unlines $ map formatLogEntry (reverse $ logEntries hist)
    TIO.appendFile (logFile hist) logContent

    TIO.writeFile (historyFile hist) $ T.unlines (reverse $ recentCommands hist)

loadHistory :: FilePath -> FilePath -> IO GameHistoryLog
loadHistory logPath histPath = do
    hist <- initGameHistory logPath histPath
    commands <- TIO.readFile histPath `catch` \(_ :: IOError) -> return ""
    let loadedCommands = filter (not . T.null) $ T.lines commands
    return hist { recentCommands = loadedCommands }
