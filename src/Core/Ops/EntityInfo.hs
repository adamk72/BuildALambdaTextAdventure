{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Core.Ops.EntityInfo (module Core.Ops.EntityInfo) where

import Entity.Entity
import Data.Text
import qualified Data.Map as Map

  -- Now we can properly combine lookups from different maps
findLocationById :: EntityId -> World -> Maybe (Entity 'LocationT)
findLocationById lId w = case findEntityById lId w of
    (Just (SomeEntity someLoc)) -> case someLoc of
        loc@Location {} -> Just loc
        _ -> Nothing
    Nothing -> Nothing

findEntityById :: EntityId -> World -> Maybe SomeEntity
findEntityById targetId world =
    case Map.lookup targetId (locations world) of
        Just loc -> Just (SomeEntity loc)
        Nothing -> case Map.lookup targetId (actors world) of
            Just actor -> Just (SomeEntity actor)
            Nothing -> case Map.lookup targetId (items world) of
                Just item -> Just (SomeEntity item)
                Nothing   -> Nothing

getActiveActorLocation :: World -> Text
getActiveActorLocation w =
    let actorLoc = getLocationId (activeActor w)
    in case getEntityNameById actorLoc w of
        Just name -> name
        Nothing -> "Location " <> unEntityId actorLoc <> " not found"

findEntityByTag :: Text -> World -> Maybe SomeEntity
findEntityByTag t = findEntityById (EntityId t)

getEntityNameByTag :: Text -> World -> Maybe Text
getEntityNameByTag t = getEntityNameById (EntityId t)

getEntityNameById :: EntityId -> World -> Maybe Text
getEntityNameById eId world =
    case findEntityById eId world of
        Just (SomeEntity entity) -> Just (getEntityName entity)
        Nothing                  -> Nothing