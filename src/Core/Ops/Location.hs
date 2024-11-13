{-# LANGUAGE GADTs #-}
module Core.Ops.Location (module Core.Ops.Location) where
import Entity.Entity
import qualified Data.Map as Map


-- | Update an entity's location
updateLocation :: EntityId -> Entity a -> World -> World
updateLocation newLocId entity world =
    case entity of
        Actor {} -> world { actors = Map.adjust (const $ setLocationId newLocId entity) (getId entity) (actors world) }
        Item  {} -> world { items  = Map.adjust (const $ setLocationId newLocId entity) (getId entity) (items world) }
        Location {} -> world -- No update, return world unchanged for Location

getActorVisibleEntitiesAtLoc :: World -> [SomeEntity]
getActorVisibleEntitiesAtLoc w =
    let actorLoc = getLocationId (activeActor w)
    in getEntitiesAtLocation actorLoc w

getEntitiesAtLocation :: EntityId -> World -> [SomeEntity]
getEntitiesAtLocation locId world =
    let itemsAtLoc = map SomeEntity $
            Map.elems $ Map.filter (\item -> getLocationId item == locId) (items world)
        actorsAtLoc = map SomeEntity $
            Map.elems $ Map.filter (\actor -> getLocationId actor == locId) (actors world)
    in itemsAtLoc ++ actorsAtLoc