{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Core.State.Operations (module Core.State.Operations) where

import           Entity.Entity
import qualified Data.Map            as Map
import           Data.Maybe          (isJust)
import           Data.Text           (Text)
import           Prelude            hiding (getContents)

maybeToEither :: Text -> Maybe a -> Either Text a
maybeToEither err Nothing = Left err
maybeToEither _ (Just x) = Right x

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

-- Now we can properly combine lookups from different maps
findEntityById :: EntityId -> World -> Maybe SomeEntity
findEntityById targetId world =
    case Map.lookup targetId (locations world) of
        Just loc -> Just (SomeEntity loc)
        Nothing -> case Map.lookup targetId (actors world) of
            Just actor -> Just (SomeEntity actor)
            Nothing -> case Map.lookup targetId (items world) of
                Just item -> Just (SomeEntity item)
                Nothing -> Nothing

findEntityByTag :: Text -> World -> Maybe SomeEntity
findEntityByTag t = findEntityById (EntityId t)

-- Helper function to get name from SomeEntity
getEntityNameById :: EntityId -> World -> Maybe Text
getEntityNameById eId world =
    case findEntityById eId world of
        Just (SomeEntity entity) -> Just (getEntityName entity)
        Nothing -> Nothing

getActiveActorInventoryID :: World -> EntityId
getActiveActorInventoryID w = entityId (actorInventory (activeActor w))

-- | Get contents of a container entity
getInventory :: Entity a -> Maybe (EntityBase 'LocationT)
getInventory entity = case entity of
    Location {} -> Nothing
    Actor {actorInventory = inv} -> Just inv
    Item {itemInventory = invM } -> invM

-- | Check if an Entity has an inventory
hasInventory :: Entity a -> Bool
hasInventory = isJust . getInventory

-- | Get the ID of an Entity's inventory location
getInventoryId :: Entity a -> Maybe EntityId
getInventoryId entity = entityId <$> getInventory entity

-- | Create a new inventory location for an Actor
createActorInventory :: EntityId -> Text -> EntityBase 'LocationT
createActorInventory ownerId name = EntityBase
    { entityId = EntityId (unEntityId ownerId <> "-inventory")
    , entityTags = Nothing
    , entityName = name <> "'s inventory"
    }

-- | Create a new inventory location for an Item
createItemInventory :: EntityId -> Text -> EntityBase 'LocationT
createItemInventory containerId name = EntityBase
    { entityId = EntityId (unEntityId containerId <> "-contents")
    , entityTags = Nothing
    , entityName = "inside " <> name
    }

-- | Update an Actor's inventory
setActorInventory :: EntityBase 'LocationT -> Entity 'ActorT -> Entity 'ActorT
setActorInventory newInv actor = actor { actorInventory = newInv }

-- | Update an Item's inventory
setItemInventory :: Maybe (EntityBase 'LocationT) -> Entity 'ItemT -> Entity 'ItemT
setItemInventory newInv item = item { itemInventory = newInv }

-- | Check if an Entity is in another Entity's inventory
isInInventoryOf :: (Movable a) => Entity a -> Entity b -> Bool
isInInventoryOf movable container =
    case getInventoryId container of
        Just invId -> getLocation movable == invId
        Nothing -> False

getInventoryContents :: Entity a -> World -> [Entity 'ItemT]
getInventoryContents entity world =
    case getInventoryId entity of
        Nothing -> []
        Just invId ->
            filter (\item -> getLocation item == invId) $
            Map.elems $ items world

-- | Move an item into a container
moveItemToContainer :: Tagged a => Entity 'ItemT -> Entity a -> World -> Either Text World
moveItemToContainer item container world
    | isContainer container =
        Right $ updateLocation (getId container) item world
    | otherwise =
        Left $ "The " <> getName container <> " is not a container."

-- | Helper function to remove duplicates while preserving order
-- nub :: Eq a => [a] -> [a]
-- nub = List.foldr (\x acc -> if x `elem` acc then acc else x : acc) []