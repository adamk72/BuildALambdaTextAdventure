{-# LANGUAGE DeriveGeneric #-}
module Core.State.JSONTypes
    ( EntityJSON(..)
    , GameWorldJSON(..)
    ) where

import           Core.State.Location
import           Data.Aeson
import           Data.Text               (Text)
import           GHC.Generics            (Generic)

-- | JSON representation of an entity (actor or item)
data EntityJSON = EntityJSON {
    jTag              :: Text,
    jName             :: Text,
    jLocTag           :: Maybe Text,
    jHasInventorySlot :: Maybe Bool
} deriving (Show, Eq, Generic)

instance FromJSON EntityJSON where
    parseJSON = withObject "EntityJSON" $ \v ->
        EntityJSON
            <$> v .: "tag"
            <*> v .: "name"
            <*> v .: "locationTag"
            <*> v .:? "hasInventorySlot"

-- | JSON representation of the game world
data GameWorldJSON = GameWorldJSON {
    jStartingActorTag :: Text,
    jPlayableActors   :: [EntityJSON],
    jLocations        :: [Location],
    jItems            :: [EntityJSON]
} deriving (Show, Eq, Generic)

instance FromJSON GameWorldJSON where
    parseJSON = withObject "GameWorldJSON" $ \v ->
        GameWorldJSON
            <$> v .: "startingActor"
            <*> v .: "characters"
            <*> v .: "locations"
            <*> v .: "items"