{-# LANGUAGE LambdaCase #-}

import           CmdOptions         as Cmd (parse, showHelp)
import           Control.Monad      (void)
import qualified Core.Launch        as Core
import           Data.Text          as T (Text, concat, intercalate, pack,
                                          unpack)
import qualified Data.Text.IO       as TIO
import           Json               (JMetadata (..))
import           JsonProcessing     as Help (getJsonFilePaths, readAllMetadata,
                                             storyDirectory)
import           System.Environment as E (getArgs)

newtype AdventureName = AdventureName { unAdventureName :: Text }
data AdventureInfo = AdventureInfo
    { advTitle       :: Text
    , advLaunchTag   :: Text
    , advDescription :: Text
    } deriving (Show)

-- Convert metadata to our domain type
toAdventureInfo :: JMetadata -> AdventureInfo
toAdventureInfo meta = AdventureInfo
    { advTitle = title meta
    , advLaunchTag = launchTag meta
    , advDescription = description meta
    }

formatAdventureInfo :: AdventureInfo -> Text
formatAdventureInfo adv = T.concat
    [ advTitle adv
    , " ("
    , advLaunchTag adv
    , ") -"
    , advDescription adv
    ]

data Command
    = RunAdventure AdventureName
    | ShowHelp
    | InvalidAdventure Text

parseArgs :: [AdventureName] -> [String] -> Command
parseArgs validNames = \case
    ["-a", option] ->
        let name = pack option
        in if name `elem` map unAdventureName validNames
           then RunAdventure (AdventureName name)
           else InvalidAdventure name
    _ -> ShowHelp

main :: IO ()
main = do
    jsonPaths <- getJsonFilePaths storyDirectory
    metadataResults <- readAllMetadata jsonPaths

    let adventures = map toAdventureInfo metadataResults
        validNames = map (AdventureName . advLaunchTag) adventures
        formattedAdventures = map formatAdventureInfo adventures

    E.getArgs >>= \args ->
        case parseArgs validNames args of
            RunAdventure name ->
                runGameWithOption (unAdventureName name)
            InvalidAdventure name -> do
                TIO.putStrLn $ "Invalid adventure name: " <> name <> "\n"
                Cmd.showHelp (unpack $ intercalate "\n" formattedAdventures)
            ShowHelp ->
                displayHelp formattedAdventures

displayHelp :: [Text]-> IO ()
displayHelp = void . Cmd.parse . unpack . intercalate "\n"

runGameWithOption :: Text -> IO ()
runGameWithOption option = do
    TIO.putStrLn $ "Running game with option: " <> option
    runGame

runGame :: IO ()
runGame = Core.launch
