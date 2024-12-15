module CmdOptions (AdventureOptions (..), RunCommand (..), findAdventurePath, getParserInfo, options, programDesc) where

import           Core.State.JSONTypes
import           Data.Text            as T (Text, intercalate, pack, unpack)
import           Options.Applicative
import           Prettyprinter        (Pretty (pretty), indent, vsep)

newtype AdventureOptions = AdventureOptions { optCommand :: RunCommand }

data RunCommand
    = Run Text
    | Replay Text
    | ReplayFile Text FilePath
    deriving (Show)

options :: [(FilePath, Either String Metadata)] -> Parser AdventureOptions
options adventures = AdventureOptions <$> commandParser adventures

commandParser :: [(FilePath, Either String Metadata)] -> Parser RunCommand
commandParser adventures = subparser
    ( command "run" (info (runOptions adventures) (progDesc "Run an adventure"))
    <> command "replay" (info (replayOptions adventures) (progDesc "Replay an adventure"))
    <> command "replay-file" (info replayFileOptions (progDesc "Replay an adventure using a specific file"))
    )

runOptions :: [(FilePath, Either String Metadata)] -> Parser RunCommand
runOptions adventures = Run <$> adventureOption adventures

replayOptions :: [(FilePath, Either String Metadata)] -> Parser RunCommand
replayOptions adventures = Replay <$> adventureOption adventures

replayFileOptions :: Parser RunCommand
replayFileOptions = ReplayFile
    <$> strOption
        ( long "adventure"
        <> short 'a'
        <> metavar "NAME"
        <> help "Name of the adventure to replay"
        )
    <*> strOption
        ( long "file"
        <> short 'f'
        <> metavar "FILEPATH"
        <> help "File containing commands to replay"
        )

adventureOption :: [(FilePath, Either String Metadata)] -> Parser Text
adventureOption adventures = strOption
    ( long "name"
    <> short 'n'
    <> metavar "NAME"
    <> help (unpack $ "Name of the adventure. Available: " <> intercalate ", " (map getAdventureName adventures))
    )

getAdventureName :: (FilePath, Either String Metadata) -> Text
getAdventureName (_, Right md) = launchTag md
getAdventureName (path, _)     = pack path

findAdventurePath :: Text -> [(FilePath, Either String Metadata)] -> Maybe FilePath
findAdventurePath tag = foldr check Nothing
  where
    check (path, Right md) acc = if launchTag md == tag then Just path else acc
    check _ acc                = acc

programDesc :: [(FilePath, Either String Metadata)] -> String
programDesc adventures = unlines
    [ "Haskell Text Adventure Game"
    , "Available adventures:"
    ] ++ concatMap showAdventure adventures
  where
    showAdventure (_, Right md) = unpack $ "  " <> title md <> " (" <> launchTag md <> ") - " <> description md <> "\n"
    showAdventure (path, _)     = unpack $ "  " <> pack path <> " (invalid metadata)\n"

versionOption :: String -> Parser (a -> a)
versionOption ver = infoOption ver
    ( long "version"
    <> short 'v'
    <> help "Show version information"
    )

getParserInfo :: [(FilePath, Either String Metadata)] -> String -> ParserInfo AdventureOptions
getParserInfo adventures ver = info (helper <*> versionOption ver <*> options adventures)
    ( fullDesc
    <> progDesc "Run a text adventure game."
    <> header "Haskell Adventure - a journey into fun!"
    <> (progDescDoc . Just $ indent 2 $ vsep [pretty (formatAdventures adventures)])
    )
  where
    formatAdventures :: [(FilePath, Either String Metadata)] -> String
    formatAdventures advs = unlines $ "Available adventures:" : map formatAdventure advs

    formatAdventure :: (FilePath, Either String Metadata) -> String
    formatAdventure (_, Right md) = unpack $
        "  " <> title md <> " (v" <> version md <> ") by " <> author md <>
        "\n    " <> description md <>
        "\n    Launch with: --name " <> launchTag md
    formatAdventure (path, Left _) =
        "  Invalid adventure at: " ++ path
