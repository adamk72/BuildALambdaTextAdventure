{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Entity.Ops.Location (module Entity.Ops.Location) where
import qualified Data.Map      as Map
import           Data.Maybe
import           Data.Text
import           Entity.Entity
import           Prelude       as P
import           Utils

-- | Update an entity's location
updateLocation :: LocationId -> Entity a -> World -> World
updateLocation newLocId entity world =
    case entity of
        Actor {} -> world { actors = Map.adjust (const $ setLocationId newLocId entity) (getId entity) (actors world) }
        Item  {} -> world { items  = Map.adjust (const $ setLocationId newLocId entity) (getId entity) (items world) }
        Location {} -> world -- No update, return world unchanged for Location

getActorVisibleMovablesAtLoc :: World -> [Text]
getActorVisibleMovablesAtLoc w =
    let acLocId = getLocationId (activeActor w)
        MovablesRecord{movableItems = items, movableActors = actors} = getMovablesRecordByLocId acLocId w
    in P.map getName items ++ P.map getName actors

getMovablesRecordByLocId :: LocationId -> World -> MovablesRecord
getMovablesRecordByLocId locId world = MovablesRecord
    { movableItems = Map.elems $ Map.filter (\item -> getLocationId item == locId) (items world)
    , movableActors = Map.elems $ Map.filter (\actor -> getLocationId actor == locId) (actors world)
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
getActorsAtLocation locId world = movableActors $ getMovablesRecordByLocId locId world

-- If you need to format actor names at a location
getActorNamesAtLocation :: LocationId -> World -> [Text]
getActorNamesAtLocation locId world =
    P.map getName $ movableActors $ getMovablesRecordByLocId locId world

-- If you need both items and actors but want to process them separately
processLocationContents :: LocationId -> World -> Text
processLocationContents locId world =
    let MovablesRecord{movableItems = items, movableActors = actors} =
            getMovablesRecordByLocId locId world
    in "You see " <> formatActors actors <> " and " <> formatItems items
  where
    formatActors []     = "no one here"
    formatActors actors = oxfordComma (P.map getName actors)

    formatItems []    = "nothing else"
    formatItems items = oxfordComma (P.map getName items)
