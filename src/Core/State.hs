{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase     #-}

module Core.State (Character(..), Metadata(..), Location(..), GameEnvironment(..), GameWorld(..), loadGameEnvironmentJSON) where

import           Control.Monad        (mzero)
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
  charTag         :: Text,
  charName        :: Text,
  currentLocation :: Location
} deriving (Show, Generic)

-- Another way of doing the mapping:
instance FromJSON Character where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = \case
        "currentLocation" -> "startingLocation"
        "charTag" -> "tag"
        "charName" -> "name"
        other -> other
    }

data Location = Location {
  locTag  :: Text,
  locName :: Text
} deriving (Show, Generic)

instance FromJSON Location where
  parseJSON (Object v) =
    Location <$> v .: "tag"
             <*> v .: "name"
  parseJSON _ = mzero

data GameWorld = GameWorld {
  activeCharacter    :: Character,
  playableCharacters :: [Character],
  locations          :: [Location]
} deriving (Show, Generic)

instance FromJSON GameWorld where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = \case
        "activeCharacter" -> "startingCharacter"
        other -> other
    }

data GameEnvironment = GameEnvironment {
    metadata :: Metadata,
    world    :: GameWorld
} deriving (Show, Generic, FromJSON)

loadGameEnvironmentJSON :: FilePath -> IO (Either String GameEnvironment)
loadGameEnvironmentJSON filePath = do
  jsonData <- B.readFile filePath
  case eitherDecode jsonData of
        Left err -> return $ Left $ "Error parsing JSON: " ++ err
        Right worldData -> do
            return $ Right worldData