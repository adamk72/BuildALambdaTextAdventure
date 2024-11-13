{-# LANGUAGE LambdaCase #-}

module Main (main) where

import           CmdOptions           as Cmd (parse, showHelp)
import           Control.Monad        (forM_, void)
import qualified Core.Launch          as Core
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
    | ShowHelp
    | InvalidAdventure Text

parseArgs :: [(FilePath, Either String Metadata)] -> [String] -> Command
parseArgs adventures = \case
    ["-a", option] ->
        let tag = pack option
        in case findAdventureByTag tag adventures of
            Just path -> RunAdventure path
            Nothing   -> InvalidAdventure tag
    _ -> ShowHelp
  where
    findAdventureByTag tag = fmap fst . find (\(_, Right md) -> launchTag md == tag)

formatMetadata :: Metadata -> Text
formatMetadata md = title md <> " (" <> launchTag md <> ") - " <> description md

main :: IO ()
main = do
    jsonPaths <- getJsonFilePaths storyDirectory
    adventures <- readAllMetadata jsonPaths

    -- Handle any loading errors first
    forM_ adventures $ \(path, result) ->
        case result of
            Left err -> TIO.putStrLn $ "Error loading " <> pack path <> ": " <> pack err
            Right _  -> return ()

    -- Get only the valid adventures
    let validAdventures = filter (isRight . snd) adventures
        adventureDescriptions = map (formatMetadata . (\(Right md) -> md) . snd) validAdventures

    getArgs >>= \args ->
        case parseArgs validAdventures args of
            RunAdventure path -> runGameWithOption path
            InvalidAdventure name -> do
                TIO.putStrLn $ "Invalid adventure name: " <> name <> "\n"
                Cmd.showHelp (unpack $ intercalate "\n" adventureDescriptions)
            ShowHelp ->
                displayHelp adventureDescriptions

displayHelp :: [Text] -> IO ()
displayHelp = void . Cmd.parse . unpack . intercalate "\n"

runGameWithOption :: FilePath -> IO ()
runGameWithOption option = do
   result <- Core.launch option
   case result of
        Left msg -> do
            TIO.putStrLn $ "Game ended with message: " <> msg
            exitWith (ExitFailure 1)
        Right () -> do
            TIO.putStrLn "Thanks for playing!"
            exitSuccess
