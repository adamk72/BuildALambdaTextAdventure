module Main (main) where

import           CmdOptions
import           Control.Monad        (forM_)
import           Core.Launch          (launch)
import           Core.State.JSONTypes (Metadata (..))
import           Data.Text
import qualified Data.Text.IO         as TIO
import           JsonProcessing       (getJsonFilePaths, readAllMetadata, storyDirectory)
import           Options.Applicative
import           Prelude              as P
import           System.Exit          (exitFailure, exitSuccess)

main :: IO ()
main = do
    jsonPaths <- getJsonFilePaths storyDirectory
    adventures <- readAllMetadata jsonPaths

    forM_ adventures $ \(path, result) ->
        case result of
            Left err -> TIO.putStrLn $ "Error loading " <> pack path <> ": " <> pack err
            Right _  -> return ()

    let version = "0.1.0.0" -- Todo: centralize this later
    whichCommand <- execParser $ getParserInfo adventures version
    case whichCommand of
        AdventureOptions cmd -> runCommand adventures cmd

runCommand :: [(FilePath, Either String Metadata)] -> RunCommand -> IO ()
runCommand adventures cmd = case cmd of
    Run name             -> runGame adventures name False Nothing
    Replay name          -> runGame adventures name True Nothing
    ReplayFile name file -> runGame adventures name True (Just file)

runGame :: [(FilePath, Either String Metadata)] -> Text -> Bool -> Maybe FilePath -> IO ()
runGame adventures name replayMode fp =
    case findAdventurePath name adventures of
        Nothing -> do
            TIO.putStrLn $ "Invalid adventure name: " <> name
            exitFailure
        Just path -> do
            result <- launch path replayMode fp
            case result of
                Left msg -> do
                    TIO.putStrLn $ "Game ended with error: " <> msg
                    exitFailure
                Right () -> do
                    TIO.putStrLn "Thanks for playing!"
                    exitSuccess
