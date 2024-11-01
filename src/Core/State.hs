{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase     #-}

module Core.State (Character(..), Metadata(..), Location(..), GameEnvironment(..), GameWorld(..), loadGameEnvironmentJSON) where

import           Control.Monad        (mzero)
import           Data.Aeson
import           Data.Aeson.Types     (Parser)
import qualified Data.ByteString.Lazy as B
import qualified Data.List as List
import           Data.Text            (Text)
import           GHC.Generics         (Generic)

data Metadata = Metadata {
    title       :: Text,
    launchTag   :: Text,
    description :: Text,
    version     :: Text,
    author      :: Text
} deriving (Show, Eq, Generic, FromJSON)

data Character = Character {
  charTag             :: Text,
  charName            :: Text,
  startingLocationTag :: Maybe Text,
  currentLocation     :: Location
} deriving (Show, Eq, Generic)

instance FromJSON Character where
  parseJSON = withObject "Character" $ \v -> do
    tag <- v .: "tag"
    name <- v .: "name"
    maybeLocTag <- v .:? "startingLocationTag"

    -- For now, we'll just store the tag and let GameWorld resolve the location
    return Character
      { charTag = tag
      , charName = name
      , startingLocationTag = maybeLocTag
      , currentLocation = error "Location not yet resolved"  -- This will be filled in by GameWorld
      }

data Location = Location {
  locTag  :: Text,
  locName :: Text
} deriving (Show, Eq, Generic)

instance FromJSON Location where
  parseJSON (Object v) =
    Location <$> v .: "tag"
             <*> v .: "name"
  parseJSON _ = mzero

data GameWorld = GameWorld {
  activeCharacter    :: Character,
  playableCharacters :: [Character],
  locations          :: [Location]
} deriving (Show, Eq, Generic)

resolveCharacterLocation :: [Location] -> Character -> Parser Character
resolveCharacterLocation locs char =
  case startingLocationTag char of
    Just targetTag ->
      case List.find (\loc -> locTag loc == targetTag) locs of
        Just loc -> return char { currentLocation = loc }
        Nothing -> fail $ "Location with tag " ++ show targetTag ++ " not found"
    Nothing -> fail "No starting location tag provided"

-- Check that the characters are actually starting in valid locations, based on their location tags.
instance FromJSON GameWorld where
  parseJSON = withObject "GameWorld" $ \v -> do
    locs <- v .: "locations"
    rawStartingChar <- v .: "startingCharacter"
    rawPlayableChars <- v .: "playableCharacters"

    startingChar <- resolveCharacterLocation locs rawStartingChar
    playableChars <- mapM (resolveCharacterLocation locs) rawPlayableChars

    return GameWorld
      { activeCharacter = startingChar
      , playableCharacters = playableChars
      , locations = locs
      }

data GameEnvironment = GameEnvironment {
    metadata :: Metadata,
    world    :: Maybe GameWorld
} deriving (Show, Eq, Generic, FromJSON)

loadGameEnvironmentJSON :: FilePath -> IO (Either String GameEnvironment)
loadGameEnvironmentJSON filePath = do
  jsonData <- B.readFile filePath
  case eitherDecode jsonData of
        Left err -> return $ Left $ "Error parsing JSON: " ++ err
        Right worldData -> do
            return $ Right worldData