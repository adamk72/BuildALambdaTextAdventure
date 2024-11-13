{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
module Core.State.Operations (module Core.State.Operations) where

import           Entity.Entity
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Maybe          (catMaybes, listToMaybe)
import           Data.Text           (Text)
import           Control.Applicative ((<|>))
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

-- | Get all entities at a specific location
getEntitiesAtLocation :: EntityId -> World -> [AnyMovableEntity]
getEntitiesAtLocation locId world =
    let itemsAtLoc  = Map.elems $ Map.filter (\item -> itemLocation item == locId) (items world)
        actorsAtLoc = Map.elems $ Map.filter (\actor -> actorLocation actor == locId) (actors world)
    in map AnyMovableItem itemsAtLoc ++ map AnyMovableActor actorsAtLoc

findEntityById :: EntityId -> Text
findEntityById = unEntityId

-- | Find an entity by its tag
findEntityByTag :: Text -> World -> Maybe AnyEntity
findEntityByTag targetTag world =
    let findInMap :: Tagged a => Map EntityId (Entity a) -> (Entity a -> AnyEntity) -> Maybe AnyEntity
        findInMap m wrapper = listToMaybe [wrapper e | e <- Map.elems m, getTag e == targetTag]
        itemMatch  = findInMap (items world) AnyItem
        actorMatch = findInMap (actors world) AnyActor
        locationMatch = findInMap (locations world) AnyLocation
    in itemMatch <|> actorMatch <|> locationMatch

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
itemExistsAtLoc :: Text -> EntityId -> World -> Bool -> Bool
itemExistsAtLoc itemTag locId world checkContainers =
    let directItems = Map.filter (\item -> itemLocation item == locId) (items world)
        hasDirectItem = any (\item -> getTag item == itemTag) (Map.elems directItems)
        containerItems = if checkContainers
                        then concatMap (`getContainerContents` world) $
                             List.filter isContainer $ Map.elems directItems
                        else []
        hasContainerItem = any (\item -> getTag item == itemTag) containerItems
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