{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Entity.Ops.Location (module Entity.Ops.Location) where
import Entity.Entity
import qualified Data.Map as Map
import Data.Text
import Prelude as P
import Utils
import Data.Maybe

-- | Update an entity's location
updateLocation :: LocationId -> Entity a -> World -> World
updateLocation newLocId entity world =
    case entity of
        Actor {} -> world { actors = Map.adjust (const $ setLocationId newLocId entity) (getId entity) (actors world) }
        Item  {} -> world { items  = Map.adjust (const $ setLocationId newLocId entity) (getId entity) (items world) }
        Location {} -> world -- No update, return world unchanged for Location

getActorVisibleEntitiesAtLoc :: World -> [Text]
getActorVisibleEntitiesAtLoc w =
    let acLocId = getLocationId (activeActor w)
        MovablesAtLocation{locationItems = items, locationActors = actors} = getMovablesAtLocationId acLocId w
    in P.map getName items ++ P.map getName actors

data MovablesAtLocation = MovablesAtLocation
    { locationItems :: [Entity 'ItemT]
    , locationActors :: [Entity 'ActorT]
    }

getMovablesAtLocationId :: LocationId -> World -> MovablesAtLocation
getMovablesAtLocationId locId world = MovablesAtLocation
    { locationItems = Map.elems $ Map.filter (\item -> getLocationId item == locId) (items world)
    , locationActors = Map.elems $ Map.filter (\actor -> getLocationId actor == locId) (actors world)
    }

getActiveActorLocation :: World -> Entity 'LocationT
getActiveActorLocation w =
    let actorLocId = getLocationId (activeActor w)
    in fromJust (findLocationById actorLocId w)

getActiveActorLocationName :: World -> Text
getActiveActorLocationName w = getName (getActiveActorLocation w)

getActiveActorLocationId :: World -> LocationId
getActiveActorLocationId w = getEntityId (getActiveActorLocation w)

getActorsAtLocation :: LocationId -> World -> [Entity 'ActorT]
getActorsAtLocation locId world = locationActors $ getMovablesAtLocationId locId world

-- If you need to format actor names at a location
getActorNamesAtLocation :: LocationId -> World -> [Text]
getActorNamesAtLocation locId world =
    P.map getName $ locationActors $ getMovablesAtLocationId locId world

-- If you need both items and actors but want to process them separately
processLocationContents :: LocationId -> World -> Text
processLocationContents locId world =
    let MovablesAtLocation{locationItems = items, locationActors = actors} =
            getMovablesAtLocationId locId world
    in "You see " <> formatActors actors <> " and " <> formatItems items
  where
    formatActors [] = "no one here"
    formatActors actors = oxfordComma (P.map getName actors)

    formatItems [] = "nothing else"
    formatItems items = oxfordComma (P.map getName items)
