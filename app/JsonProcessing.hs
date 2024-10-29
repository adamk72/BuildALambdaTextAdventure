{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE TupleSections #-}

module JsonProcessing (storyDirectory, getJsonFilePaths, readAllMetadata) where

import           Data.Aeson           (eitherDecode)
import qualified Data.ByteString.Lazy as B
import qualified Data.Either        as Either
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


readMetadata :: FilePath -> IO (Either String JMetadata)
readMetadata filePath = do
    content <- B.readFile filePath
    return $ metadata <$> eitherDecode content

readAllMetadata :: [Either String FilePath] -> IO [(FilePath, JMetadata)]
readAllMetadata filePaths = do
    let validPaths = Either.rights filePaths -- filter out the bad files
    results <- mapM processFile validPaths
    return $ Either.rights results -- filter out the bad metadata
  where
    processFile filePath = do
      md <- readMetadata filePath
      return $ (filePath,) <$> md

