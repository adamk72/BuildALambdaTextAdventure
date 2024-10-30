{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Core.State (Metadata(..), Location(..), GameEnvironment(..), GameWorld(..), loadGameEnvironmentJSON) where

import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           Data.Text            as T
import           GHC.Generics         (Generic)

data Metadata = Metadata {
    title       :: Text,
    launchTag   :: Text,
    description :: Text,
    version     :: Text,
    author      :: Text
} deriving (Show, Generic, FromJSON)

data Character = Character {
  tag              :: Text,
  name             :: Text,
  startingLocation :: Text
} deriving (Show, Generic, FromJSON)

data Location = Location {
  ltag  :: Text,
  lname :: Text
} deriving (Show, Generic, FromJSON)

data GameWorld = GameWorld {
  startingCharacter  :: Text,
  playableCharacters :: [Character],
  locations          :: [Location]
} deriving (Show, Generic, FromJSON)

data GameEnvironment = GameEnvironment {
    metadata :: Metadata,
    world    :: GameWorld
} deriving (Show, Generic, FromJSON)

-- initialWorld :: GameEnvironment
-- initialWorld = GameEnvironment $ GameWorld $ Character "alice" "Alice" (Location "meadow" "The Meadow")

loadGameEnvironmentJSON :: FilePath -> IO (Either String GameEnvironment)
loadGameEnvironmentJSON filePath = do
  jsonData <- B.readFile filePath
  case eitherDecode jsonData of
        Left err -> return $ Left $ "Error parsing JSON: " ++ err
        Right worldData -> do
            return $ Right worldData

