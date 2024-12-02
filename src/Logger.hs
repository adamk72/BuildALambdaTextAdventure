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

data LogEntry = LogEntry
    { timestamp :: UTCTime
    , level     :: LogLevel
    , message   :: T.Text
    , isCommand :: Bool
    } deriving (Show, Eq)

data GameHistory = GameHistory
    { logEntries        :: [LogEntry]
    , recentCommands    :: [T.Text]
    , maxCommandHistory :: Int
    , logFile           :: FilePath
    , historyFile       :: FilePath
    } deriving (Show)

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

logCommand :: MonadIO m => GameHistory -> T.Text -> m GameHistory
logCommand hist cmd = do
    timestamp <- liftIO getCurrentTime
    let entry = LogEntry timestamp Info cmd True
        newHist = hist
            { logEntries = entry : logEntries hist
            , recentCommands = take (maxCommandHistory hist) (cmd : recentCommands hist)
            }
    liftIO $ TIO.appendFile (logFile hist) (formatLogEntry entry)
    return newHist

logDebug :: MonadIO m => GameHistory -> T.Text -> m GameHistory
logDebug hist = logMessage hist Debug False

logInfo :: MonadIO m => GameHistory -> T.Text -> m GameHistory
logInfo hist = logMessage hist Info False

logError :: MonadIO m => GameHistory -> T.Text -> m GameHistory
logError hist = logMessage hist Error False

logMessage :: MonadIO m => GameHistory -> LogLevel -> Bool -> T.Text -> m GameHistory
logMessage hist level isCmd msg = do
    timestamp <- liftIO getCurrentTime
    let entry = LogEntry timestamp level msg isCmd
        newHist = hist { logEntries = entry : logEntries hist }
    -- Write to log file
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

getRecentCommands :: GameHistory -> [T.Text]
getRecentCommands = recentCommands

saveHistory :: GameHistory -> IO ()
saveHistory hist = do
    let logContent = T.unlines $ map formatLogEntry (reverse $ logEntries hist)
    TIO.appendFile (logFile hist) logContent

    TIO.writeFile (historyFile hist) $ T.unlines (reverse $ recentCommands hist)

loadHistory :: FilePath -> FilePath -> IO GameHistory
loadHistory logPath histPath = do
    hist <- initGameHistory logPath histPath
    -- Load previous commands if they exist
    commands <- TIO.readFile histPath `catch` \(_ :: IOError) -> return ""
    let loadedCommands = filter (not . T.null) $ T.lines commands
    return hist { recentCommands = loadedCommands }
