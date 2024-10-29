module GameWorld where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-- Location Types
data LocationType =
    Dungeon
    | Forest
    | Cave
    | Village
    | Castle
    deriving (Show, Eq)

-- Direction for navigation
data Direction =
    North
    | South
    | East
    | West
    | Up
    | Down
    deriving (Show, Eq)

-- Location structure
data Location = Location {
    locId :: String,
    locName :: String,
    locType :: LocationType,
    locDescription :: String,
    locConnections :: [(Direction, String)]  -- (Direction, destination locId)
} deriving (Show)

-- Game World type
newtype GameWorld = GameWorld {
    locations :: Map.Map String Location
} deriving (Show)

-- Create a new empty game world
emptyWorld :: GameWorld
emptyWorld = GameWorld Map.empty

-- Add a location to the world
addLocation :: GameWorld -> Location -> GameWorld
addLocation (GameWorld locs) loc =
    GameWorld $ Map.insert (locId loc) loc locs

-- Connect two locations bidirectionally
connectLocations :: GameWorld -> String -> String -> Direction -> GameWorld
connectLocations world id1 id2 dir =
    let opposite = case dir of
            North -> South
            South -> North
            East -> West
            West -> East
            Up -> Down
            Down -> Up
        addConnection loc1 loc2 direction =
            loc1 { locConnections = (direction, locId loc2) : locConnections loc1 }
        updatedWorld = case (Map.lookup id1 (locations world), Map.lookup id2 (locations world)) of
            (Just loc1, Just loc2) ->
                let newLoc1 = addConnection loc1 loc2 dir
                    newLoc2 = addConnection loc2 loc1 opposite
                    newLocs = Map.insert id2 newLoc2 $ Map.insert id1 newLoc1 (locations world)
                in GameWorld newLocs
            _ -> world
    in updatedWorld

-- Get available exits from a location
getExits :: GameWorld -> String -> [(Direction, String)]
getExits (GameWorld locs) locId =
    maybe [] locConnections (Map.lookup locId locs)

-- Example usage: Creating a small world
exampleWorld :: GameWorld
exampleWorld =
    let entrance = Location "entrance" "Cave Entrance" Cave
            "A dark cave mouth opens before you, promising adventure." []
        mainHall = Location "hall" "Main Hall" Dungeon
            "A vast hall with ancient pillars stretching into darkness." []
        forest = Location "forest" "Dense Forest" Forest
            "Thick trees surround you, their branches blocking most sunlight." []

        world1 = addLocation emptyWorld entrance
        world2 = addLocation world1 mainHall
        world3 = addLocation world2 forest

        -- Connect locations
        world4 = connectLocations world3 "entrance" "hall" North
        finalWorld = connectLocations world4 "entrance" "forest" East
    in finalWorld

-- Helper function to get location description
getLocationDesc :: GameWorld -> String -> String
getLocationDesc (GameWorld locs) locId =
    maybe "Location not found" locDescription (Map.lookup locId locs)
