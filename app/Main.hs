{-# LANGUAGE LambdaCase #-}

module Main (main) where

import           CmdOptions           as Cmd (parse, showHelp)
import           Control.Monad        (forM_, void)
import           Core.Launch          (launch)
import           Core.State.JSONTypes (Metadata (..))
import           Data.Either          (isRight)
import           Data.List            (find)
import           Data.Text            (Text, intercalate, pack, unpack)
import qualified Data.Text.IO         as TIO
import           JsonProcessing       (getJsonFilePaths, readAllMetadata, storyDirectory)
import           System.Environment   (getArgs)
import           System.Exit          (ExitCode (ExitFailure), exitSuccess, exitWith)

data Command
    = RunAdventure FilePath
    | ReplayAdventure FilePath
    | ShowHelp
    | InvalidAdventure Text

parseArgs :: [(FilePath, Either String Metadata)] -> [String] -> Command
parseArgs adventures = \case
    ["-a", option] -> findAdventureCommand RunAdventure option
    ["-r", option] -> findAdventureCommand ReplayAdventure option
    _ -> ShowHelp
  where
    findAdventureCommand constructor option =
        let tag = pack option
        in case findAdventureByTag tag adventures of
            Just path -> constructor path
            Nothing   -> InvalidAdventure tag

    findAdventureByTag tag = fmap fst . find (\(_, result) -> either (const False) ((== tag) . launchTag) result)

formatMetadata :: Metadata -> Text
formatMetadata md = title md <> " (" <> launchTag md <> ") - " <> description md

main :: IO ()
main = do
    jsonPaths <- getJsonFilePaths storyDirectory
    adventures <- readAllMetadata jsonPaths

    forM_ adventures $ \(path, result) ->
        case result of
            Left err -> TIO.putStrLn $ "Error loading " <> pack path <> ": " <> pack err
            Right _  -> return ()

    let validAdventures = filter (isRight . snd) adventures
        adventureDescriptions = map (formatMetadata . either (error "No valid adventures found.") id . snd) validAdventures

    getArgs >>= \args ->
        case parseArgs validAdventures args of
            RunAdventure path -> runGameWithOption path False
            ReplayAdventure path -> runGameWithOption path True
            InvalidAdventure name -> do
                TIO.putStrLn $ "Invalid adventure name: " <> name <> "\n"
                Cmd.showHelp (unpack $ intercalate "\n" adventureDescriptions)
            ShowHelp ->
                displayHelp adventureDescriptions

displayHelp :: [Text] -> IO ()
displayHelp = void . Cmd.parse . unpack . intercalate "\n"

runGameWithOption :: FilePath -> Bool -> IO ()
runGameWithOption option replayMode = do
   result <- launch option replayMode
   case result of
        Left msg -> do
            TIO.putStrLn $ "Game ended with message: " <> msg
            exitWith (ExitFailure 1)
        Right () -> do
            TIO.putStrLn "Thanks for playing!"
            exitSuccess
