module Core.Ops.Location (module Core.Ops.Location) where


-- | Update an entity's location
updateLocation :: EntityId -> Entity a -> World -> World
updateLocation newLocId entity world =
    case entity of
        Actor {} -> world { actors = Map.adjust (const $ setLocation newLocId entity) (getId entity) (actors world) }
        Item  {} -> world { items  = Map.adjust (const $ setLocation newLocId entity) (getId entity) (items world) }
        Location {} -> world -- No update, return world unchanged for Location

getEntitiesAtLocation :: EntityId -> World -> [SomeEntity]
getEntitiesAtLocation locId world =
    let itemsAtLoc = map SomeEntity $
            Map.elems $ Map.filter (\item -> getLocation item == locId) (items world)
        actorsAtLoc = map SomeEntity $
            Map.elems $ Map.filter (\actor -> getLocation actor == locId) (actors world)
    in itemsAtLoc ++ actorsAtLoc