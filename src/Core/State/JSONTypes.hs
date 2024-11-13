{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Core.State.JSONTypes (EntityJSON (..), Metadata (..), WorldJSON (..), Location(..)) where


import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics (Generic)

-- | JSON representation of an entity (actor or item)
data EntityJSON = EntityJSON {
    jTag              :: Text,
    jName             :: Text,
    jLocTag           :: Maybe Text,
    jHasInventorySlot :: Maybe Bool
} deriving (Show, Eq, Generic)

data Location = Location {
    locTag          :: Text,        -- Will be converted to EntityId
    locName         :: Text,
    destinationTags :: [Text]       -- Will be converted to [EntityId]
} deriving (Show, Eq, Generic, FromJSON)

instance FromJSON EntityJSON where
    parseJSON = withObject "EntityJSON" $ \v ->
        EntityJSON
            <$> v .: "tag"
            <*> v .: "name"
            <*> v .: "locationTag"
            <*> v .:? "hasInventorySlot"

-- | JSON representation of the game world
data WorldJSON = WorldJSON {
    jPlayableActors   :: [EntityJSON],
    jLocations        :: [Location],
    jItems            :: [EntityJSON],
    jStartingActorTag :: Text
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
