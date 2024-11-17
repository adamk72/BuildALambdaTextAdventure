{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Core.Ops.Location (module Core.Ops.Location) where
import Entity.Entity
import qualified Data.Map as Map
import Data.Text
import Prelude as P
import Utils


-- | Update an entity's location
updateLocation :: EntityId -> Entity a -> World -> World
updateLocation newLocId entity world =
    case entity of
        Actor {} -> world { actors = Map.adjust (const $ setLocationId newLocId entity) (getId entity) (actors world) }
        Item  {} -> world { items  = Map.adjust (const $ setLocationId newLocId entity) (getId entity) (items world) }
        Location {} -> world -- No update, return world unchanged for Location

getActorVisibleEntitiesAtLoc :: World -> [Text]
getActorVisibleEntitiesAtLoc w =
    let acLocId = getLocationId (activeActor w)
        EntitiesAtLocation{locationItems = items, locationActors = actors} = getEntitiesAtLocation acLocId w
    in P.map getName items ++ P.map getName actors

data EntitiesAtLocation = EntitiesAtLocation
    { locationItems :: [Entity 'ItemT]
    , locationActors :: [Entity 'ActorT]
    }

getEntitiesAtLocation :: EntityId -> World -> EntitiesAtLocation
getEntitiesAtLocation locId world = EntitiesAtLocation
    { locationItems = Map.elems $ Map.filter (\item -> getLocationId item == locId) (items world)
    , locationActors = Map.elems $ Map.filter (\actor -> getLocationId actor == locId) (actors world)
    }

getActiveActorLocation :: World -> Text
getActiveActorLocation w =
    let actorLocId = getLocationId (activeActor w)
    in case findLocationById actorLocId w of
        Just loc -> getName loc
        Nothing -> "Location " <> unEntityId actorLocId <> " not found"

getActorsAtLocation :: EntityId -> World -> [Entity 'ActorT]
getActorsAtLocation locId world = locationActors $ getEntitiesAtLocation locId world

-- If you need to format actor names at a location
getActorNamesAtLocation :: EntityId -> World -> [Text]
getActorNamesAtLocation locId world =
    P.map getName $ locationActors $ getEntitiesAtLocation locId world

-- If you need both items and actors but want to process them separately
processLocationContents :: EntityId -> World -> Text
processLocationContents locId world =
    let EntitiesAtLocation{locationItems = items, locationActors = actors} =
            getEntitiesAtLocation locId world
    in "You see " <> formatActors actors <> " and " <> formatItems items
  where
    formatActors [] = "no one here"
    formatActors actors = oxfordComma (P.map getName actors)

    formatItems [] = "nothing else"
    formatItems items = oxfordComma (P.map getName items)
