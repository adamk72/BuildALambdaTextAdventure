{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Core.State.Operations (module Core.State.Operations) where

import           Entity.Entity
import qualified Data.Map            as Map
import           Data.Maybe          (catMaybes)
import           Data.Text           (Text)
import qualified Data.List           as List
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

getActorInventory :: World -> Either Text EntityId
getActorInventory world =
    Right $ actorLocation (activeActor world)

-- | Get contents of a container entity
getContainerContents :: (Container a) => Entity a -> World -> [Entity 'ItemT]
getContainerContents container world =
    let contentIds = getContents container
    in catMaybes [Map.lookup _id (items world) | _id <- contentIds]

-- | Move an item into a container
moveItemToContainer :: Tagged a => Entity 'ItemT -> Entity a -> World -> Either Text World
moveItemToContainer item container world
    | isContainer container =
        Right $ updateLocation (getId container) item world
    | otherwise =
        Left $ "The " <> getName container <> " is not a container."

-- | Check if an item exists in a location
itemTagExistsAtActorLoc :: Text -> World -> Bool
itemTagExistsAtActorLoc t = itemExistsAtActorLoc (EntityId t)

itemExistsAtActorLoc :: EntityId -> World -> Bool
itemExistsAtActorLoc itemId w = itemExistsAtLoc itemId (actorLocation (activeActor w)) w True

itemExistsAtLoc :: EntityId -> EntityId -> World -> Bool -> Bool
itemExistsAtLoc itemTag locId world checkContainers =
    let directItems = Map.filter (\item -> itemLocation item == locId) (items world)
        hasDirectItem = any (\item -> getId item == itemTag) (Map.elems directItems)
        containerItems = if checkContainers
                        then concatMap (`getContainerContents` world) $
                             List.filter isContainer $ Map.elems directItems
                        else []
        hasContainerItem = any (\item -> getId item == itemTag) containerItems
    in hasDirectItem || hasContainerItem

-- | Get all items in a location including those in containers
getItemsAtLocationDeep :: EntityId -> World -> Bool -> [Entity 'ItemT]
getItemsAtLocationDeep locId world includeContainers =
    let directItems = Map.elems $ Map.filter (\item -> itemLocation item == locId) (items world)
        containerItems = if includeContainers
                        then concatMap (`getContainerContents` world) $
                             List.filter isContainer directItems
                        else []
    in directItems ++ containerItems

-- | Helper function to remove duplicates while preserving order
nub :: Eq a => [a] -> [a]
nub = List.foldr (\x acc -> if x `elem` acc then acc else x : acc) []