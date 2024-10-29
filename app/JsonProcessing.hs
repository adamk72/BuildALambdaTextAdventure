{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module JsonProcessing (storyDirectory, getJsonFilePaths, readAllMetadata) where

import           Data.Aeson           (FromJSON, ToJSON, eitherDecode)
import qualified Data.ByteString.Lazy as B
import           Data.Text            as T (Text)
import           GHC.Generics         (Generic)
import           Json                 (JGameEnvironment(..), JMetadata(..))
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


readMetadata :: Either String FilePath -> IO (Either String JMetadata)
readMetadata (Left err) = return (Left err)
readMetadata (Right filePath) = do
    content <- B.readFile filePath
    return $ fmap metadata $ eitherDecode content

readAllMetadata :: [Either String FilePath] -> IO [Either String JMetadata]
readAllMetadata = mapM processFile
  where
    processFile file = do
      readMetadata file

