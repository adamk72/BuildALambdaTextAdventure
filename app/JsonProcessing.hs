{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module JsonProcessing (storyDirectory, getJsonFilePaths, processJsonFiles, AdventureDetail, fullName, shortName, description) where

import           Data.Aeson           (FromJSON, ToJSON, eitherDecode)
import qualified Data.ByteString.Lazy as B
import           Data.Text            as T (Text)
import           GHC.Generics         (Generic)
import           System.Directory
import           System.FilePath      (takeExtension, (</>))

data AdventureDetail =
  AdventureDetail { fullName    :: !T.Text
                  , shortName   :: !T.Text
                  , description :: !T.Text
                  } deriving (Show,Generic)

instance FromJSON AdventureDetail
instance ToJSON AdventureDetail

storyDirectory :: FilePath
storyDirectory = "stories"

getJsonFilePaths :: FilePath -> IO [Either String FilePath]
getJsonFilePaths dir = do
    allFiles <- listDirectory dir
    let jsonFiles = filter (\f -> takeExtension f == ".json") allFiles
    if null jsonFiles
    then return [Left "No adventure files found!"]
    else return (map (Right . (dir </>)) jsonFiles)


readJsonFile :: Either String FilePath -> IO (Either String AdventureDetail)
readJsonFile (Left err) = return (Left err)
readJsonFile (Right filePath) = do
    contents <- B.readFile filePath
    return (eitherDecode contents)

processJsonFiles :: [Either String FilePath] -> IO [Either String AdventureDetail]
processJsonFiles = mapM processFile
  where
    processFile file = do
      readJsonFile file

