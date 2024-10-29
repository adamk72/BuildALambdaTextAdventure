{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module GameWorld where

import qualified Data.Map as Map
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import Control.Monad (mzero)
import Data.Maybe (fromMaybe)

-- Location Types
data LocationType =
    Dungeon
    | Forest
    | Cave
    | Lake
    | Village
    | Castle
    deriving (Show, Eq, Generic)

instance FromJSON LocationType
instance ToJSON LocationType

-- Direction for navigation
data Direction =
    North
    | South
    | East
    | West
    | Up
    | Down
    deriving (Show, Eq, Generic)

instance FromJSON Direction
instance ToJSON Direction

-- Connection structure
data Connection = Connection {
    direction :: Direction,
    targetId :: String
} deriving (Show, Generic)

instance FromJSON Connection
instance ToJSON Connection

-- Location structure
data Location = Location {
    locId :: String,
    locName :: String,
    locType :: LocationType,
    locDescription :: String,
    locConnections :: [Connection]
} deriving (Show, Generic)

instance FromJSON Location
instance ToJSON Location

-- Region structure
data Region = Region {
    regionId :: String,
    regionName :: String,
    regionLocations :: Map.Map String Location
} deriving (Show, Generic)

instance FromJSON Region where
    parseJSON = withObject "Region" $ \v -> Region
        <$> v .: "id"
        <*> v .: "name"
        <*> (Map.fromList <$> ((,) <$> (v .: "id") <*> (v .: "locations")) `mzero`)

instance ToJSON Region

-- GameWorld type
data GameWorld = GameWorld {
    metadata :: Metadata,
    regions :: [Region],
    currentRegion :: String,
    currentLocation :: String
} deriving (Show)

-- Metadata type
data Metadata = Metadata {
    title :: String,
    version :: String,
    author :: String
} deriving (Show, Generic)

instance FromJSON Metadata
instance ToJSON Metadata

-- Load world from JSON file
loadWorldFromJSON :: FilePath -> IO (Either String GameWorld)
loadWorldFromJSON filePath = do
    jsonData <- B.readFile filePath
    case eitherDecode jsonData of
        Left err -> return $ Left $ "Error parsing JSON: " ++ err
        Right worldData -> do
            let metadata = worldData ^. key "metadata" . _JSON
            let regions = worldData ^. key "regions" . _JSON
            let initialRegion = case regions of
                    [] -> "unknown"
                    (firstRegion:_) -> regionId firstRegion
            let world = GameWorld metadata regions initialRegion "unknown"
            return $ Right world

-- Get available exits from a location
getExits :: GameWorld -> [(Direction, String)]
getExits world = case Map.lookup (currentLocation world) (regionLocations (getCurrentRegion world)) of
    Just loc -> locConnections loc
    Nothing -> []

-- Get location description
getLocationDesc :: GameWorld -> String
getLocationDesc world = case Map.lookup (currentLocation world) (regionLocations (getCurrentRegion world)) of
    Just loc -> locDescription loc
    Nothing -> "Location not found"

-- Get current region
getCurrentRegion :: GameWorld -> Region
getCurrentRegion world = case find ((== currentRegion world) . regionId) (regions world) of
    Just region -> region
    Nothing -> error "Current region not found"