{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module GameConfigConverter where

import Core.State.Entity
import Core.State.GameState
import Core.State.JSON
import Core.State.Location
import Core.State.TaggedEntity
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import qualified Data.Text.IO as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeBaseName, splitDirectories)
import Data.List (intercalate)
import Data.Char (toUpper)

-- | Convert first character of string to uppercase
capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

-- | Generate module name from output path and base filename
generateModuleName :: FilePath -> String -> String
generateModuleName outputDir baseName =
    let dirs = splitDirectories outputDir
        -- Filter out "." and "..", empty strings, and "test" folder
        validDirs = filter (\x -> not (x `elem` [".", "..", "", "test"])) dirs
        -- Capitalize each directory and the base name
        moduleComponents = map capitalize (validDirs ++ [baseName ++ "Spec"])
    in intercalate "." moduleComponents

-- | Convert a JSON game file into both a GameEnvironment and test configuration
processGameConfig :: FilePath -> FilePath -> IO ()
processGameConfig inputPath outputDir = do
    -- Create output directory if it doesn't exist
    createDirectoryIfMissing True outputDir

    -- Load and parse the game environment
    result <- loadGameEnvironmentJSON inputPath
    case result of
        Left err -> putStrLn $ "Error processing file: " ++ err
        Right gameEnv -> do
            -- Generate the test configuration
            let baseName = capitalize $ takeBaseName inputPath
            let moduleName = generateModuleName outputDir baseName
            let testConfig = generateTestConfig moduleName gameEnv

            -- Write test configuration to file
            let testFilePath = outputDir </> baseName ++ "Spec.hs"  -- Changed Test to Spec here
            writeFile testFilePath testConfig

-- | Generate a Hspec test module from a GameEnvironment
generateTestConfig :: String -> GameEnvironment -> String
generateTestConfig moduleName GameEnvironment{..} =
    let worldConfig = case world of
            Nothing -> "    { world = Nothing }"
            Just w -> unlines
                [ "    { world = Just $ GameWorld"
                , "        { gwActiveActor = " ++ show (gwActiveActor w)
                , "        , gwPlayableActors = " ++ show (gwPlayableActors w)
                , "        , gwLocations = " ++ show (gwLocations w)
                , "        , gwItems = " ++ show (gwItems w)
                , "        }"
                , "    }"
                ]

        specDefinition = unlines
            [ "spec :: Spec"
            , "spec = do"
            , "    describe \"Game Configuration\" $ do"
            , "        it \"has correct metadata\" $ do"
            , "            metadata testGameEnv `shouldBe` " ++ show metadata
            , ""
            , generateWorldTests world
            ]
    in unlines
        [ "{-# LANGUAGE OverloadedStrings #-}"
        , ""
        , "module " ++ moduleName ++ " (spec) where"
        , ""
        , "import Test.Hspec"
        , "import Core.State.Entity"
        , "import Core.State.GameState"
        , "import Core.State.Location"
        , "import Core.State.TaggedEntity"
        , "import Data.Text (Text)"
        , ""
        , "-- Test GameEnvironment configuration"
        , "testGameEnv :: GameEnvironment"
        , "testGameEnv = GameEnvironment"
        , "    { metadata = " ++ show metadata
        , worldConfig
        , ""
        , specDefinition
        ]

-- | Generate specific tests for the GameWorld
generateWorldTests :: Maybe GameWorld -> String
generateWorldTests Nothing =
    "        it \"has no world defined\" $ do\n" ++
    "            world testGameEnv `shouldBe` Nothing"
generateWorldTests (Just GameWorld{..}) = unlines
    [ "        it \"has correct active actor\" $ do"
    , "            gwActiveActor <$> world testGameEnv `shouldBe` Just " ++ show gwActiveActor
    , ""
    , "        it \"has expected number of playable actors\" $ do"
    , "            length . gwPlayableActors <$> world testGameEnv `shouldBe` Just " ++ show (length gwPlayableActors)
    , ""
    , "        it \"has expected number of locations\" $ do"
    , "            length . gwLocations <$> world testGameEnv `shouldBe` Just " ++ show (length gwLocations)
    , ""
    , "        it \"has expected number of items\" $ do"
    , "            length . gwItems <$> world testGameEnv `shouldBe` Just " ++ show (length gwItems)
    ]

-- | Main function to run the converter
main :: IO ()
main = do
    putStrLn "Game Configuration Converter"
    putStrLn "Enter input JSON file path:"
    inputPath <- getLine
    putStrLn "Enter output directory for test configuration:"
    outputDir <- getLine
    processGameConfig inputPath outputDir