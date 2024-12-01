{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Core.State.JSONTypes (EntityJSON (..), Location (..), Metadata (..), WorldJSON (..)) where

import           Data.Aeson
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import           Scenario.ScenarioConverter (ScenarioJSON)

-- | JSON representation of an entity (actor or item)
data EntityJSON = EntityJSON {
    jTag              :: Text,
    jTags             :: Maybe [Text],
    jName             :: Text,
    jLocTag           :: Maybe Text,
    jHasInventorySlot :: Maybe Bool
} deriving (Show, Eq, Generic)

instance FromJSON EntityJSON where
    parseJSON = withObject "EntityJSON" $ \v -> do
        tag <- v .: "tag"
        tags <- v .:? "tags"
        name <- v .: "name"
        locTag <- v .: "locationTag"
        hasInv <- v .:? "hasInventorySlot"
        return EntityJSON
            { jTag = tag
            , jTags = tags
            , jName = name
            , jLocTag = locTag
            , jHasInventorySlot = hasInv
            }

data Location = Location {
    locTag          :: Text,        -- Will be converted to EntityId
    locTags         :: Maybe [Text],
    locName         :: Text,
    destinationTags :: [Text]       -- Will be converted to [EntityId]
} deriving (Show, Eq, Generic)

instance FromJSON Location where
    parseJSON = withObject "Location" $ \v -> do
        tag <- v .: "tag"
        tags <- v .:? "tags"
        name <- v .: "name"
        dests <- v .: "destinationTags"
        return Location
            { locTag = tag
            , locTags = tags
            , locName = name
            , destinationTags = dests
            }

-- | JSON representation of the game world
data WorldJSON = WorldJSON {
    jStartingActorTag :: Text,
    jPlayableActors   :: [EntityJSON],
    jLocations        :: [Location],
    jItems            :: [EntityJSON],
    jScenarios        :: Maybe [ScenarioJSON]
} deriving (Show, Eq, Generic)

instance FromJSON WorldJSON where
    parseJSON = withObject "WorldJSON" $ \v -> do
        startActor <- v .: "startingActor"
        actors <- v .: "characters"
        locs <- v .: "locations"
        items <- v .: "items"
        scenarios <- v .:? "scenarios"  -- Optional field
        return WorldJSON
            { jStartingActorTag = startActor
            , jPlayableActors = actors
            , jLocations = locs
            , jItems = items
            , jScenarios = scenarios
            }

data Metadata = Metadata {
    title       :: Text,
    launchTag   :: Text,
    description :: Text,
    version     :: Text,
    author      :: Text
} deriving (Show, Eq, Generic, FromJSON)
