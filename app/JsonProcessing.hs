{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module JsonProcessing (storyDirectory, getJsonFilePaths, processJsonFiles) where

import           Data.Aeson           (FromJSON, ToJSON, eitherDecode)
import qualified Data.ByteString.Lazy as B
import           Data.Text            as T (Text)
import           GHC.Generics         (Generic)
import           Json                 (JGameEnvironment)
import           System.Directory
import           System.FilePath      (takeExtension, (</>))

storyDirectory :: FilePath
storyDirectory = "stories"

getJsonFilePaths :: FilePath -> IO [Either String FilePath]
getJsonFilePaths dir = do
    allFiles <- listDirectory dir
    let jsonFiles = filter (\f -> takeExtension f == ".json") allFiles
    if null jsonFiles
    then return [Left "No adventure files found!"]
    else return (map (Right . (dir </>)) jsonFiles)


readJsonFile :: Either String FilePath -> IO (Either String JGameEnvironment)
readJsonFile (Left err) = return (Left err)
readJsonFile (Right filePath) = do
    contents <- B.readFile filePath
    return (eitherDecode contents)

processJsonFiles :: [Either String FilePath] -> IO [Either String JGameEnvironment]
processJsonFiles = mapM processFile
  where
    processFile file = do
      readJsonFile file

