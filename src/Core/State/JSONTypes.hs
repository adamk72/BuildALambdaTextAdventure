{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Core.State.JSONTypes (EntityJSON (..), Location (..), Metadata (..), WorldJSON (..)) where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics (Generic)

-- | JSON representation of an entity (actor or item)
data EntityJSON = EntityJSON {
    jTag              :: Text,
    jTags             :: Maybe [Text],
    jName             :: Text,
    jLocTag           :: Maybe Text,
    jHasInventorySlot :: Maybe Bool
} deriving (Show, Eq, Generic)

instance FromJSON EntityJSON where
    parseJSON = withObject "EntityJSON" $ \v ->
        EntityJSON
            <$> v .: "tag"
            <*> v .:? "tags"
            <*> v .: "name"
            <*> v .: "locationTag"
            <*> v .:? "hasInventorySlot"

data Location = Location {
    locTag          :: Text,        -- Will be converted to EntityId
    locTags         :: Maybe [Text],
    locName         :: Text,
    destinationTags :: [Text]       -- Will be converted to [EntityId]
} deriving (Show, Eq, Generic)

instance FromJSON Location where
    parseJSON = withObject "Location" $ \v ->
        Location
            <$> v .: "tag"
            <*> v .:? "tags"
            <*> v .: "name"
            <*> v .: "destinationTags"

-- | JSON representation of the game world
data WorldJSON = WorldJSON {
    jStartingActorTag :: Text,
    jPlayableActors   :: [EntityJSON],
    jLocations        :: [Location],
    jItems            :: [EntityJSON]
} deriving (Show, Eq, Generic)

instance FromJSON WorldJSON where
    parseJSON = withObject "WorldJSON" $ \v ->
        WorldJSON
            <$> v .: "startingActor"
            <*> v .: "characters"
            <*> v .: "locations"
            <*> v .: "items"

data Metadata = Metadata {
    title       :: Text,
    launchTag   :: Text,
    description :: Text,
    version     :: Text,
    author      :: Text
} deriving (Show, Eq, Generic, FromJSON)