module Core.Ops.EntityInfo (module Core.Ops.Entity) where

  -- Now we can properly combine lookups from different maps
findEntityById :: EntityId -> World -> Maybe SomeEntity
findEntityById targetId world =
    case Map.lookup targetId (locations world) of
        Just loc -> Just (SomeEntity loc)
        Nothing -> case Map.lookup targetId (actors world) of
            Just actor -> Just (SomeEntity actor)
            Nothing -> case Map.lookup targetId (items world) of
                Just item -> Just (SomeEntity item)
                Nothing   -> Nothing

findEntityByTag :: Text -> World -> Maybe SomeEntity
findEntityByTag t = findEntityById (EntityId t)

-- Helper function to get name from SomeEntity
getEntityNameById :: EntityId -> World -> Maybe Text
getEntityNameById eId world =
    case findEntityById eId world of
        Just (SomeEntity entity) -> Just (getEntityName entity)
        Nothing                  -> Nothing

  -- Now we can properly combine lookups from different maps
findEntityById :: EntityId -> World -> Maybe SomeEntity
findEntityById targetId world =
    case Map.lookup targetId (locations world) of
        Just loc -> Just (SomeEntity loc)
        Nothing -> case Map.lookup targetId (actors world) of
            Just actor -> Just (SomeEntity actor)
            Nothing -> case Map.lookup targetId (items world) of
                Just item -> Just (SomeEntity item)
                Nothing   -> Nothing

findEntityByTag :: Text -> World -> Maybe SomeEntity
findEntityByTag t = findEntityById (EntityId t)

-- Helper function to get name from SomeEntity
getEntityNameById :: EntityId -> World -> Maybe Text
getEntityNameById eId world =
    case findEntityById eId world of
        Just (SomeEntity entity) -> Just (getEntityName entity)
        Nothing                  -> Nothing