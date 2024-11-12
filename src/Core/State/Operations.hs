module Core.State.Operations (module Core.State.Operations) where

import           Entity.Entity
import           Core.State.EntityContainer
import           Core.State.GameState       (World (..))
import qualified Data.List                  as List
import           Data.Maybe
import           Data.Text                  (Text)

setEntityLoc :: Location -> Entity -> Entity
setEntityLoc newLoc entity =
    entity { entityTag = (entityTag entity) { location = newLoc } }

setActorLoc :: Location -> Actor -> Actor
setActorLoc = setEntityLoc

getActiveActorLoc :: World -> Location
getActiveActorLoc gw = location $ entityTag $ gwActiveActor gw

getActorInventory :: World -> Either Text EntityId
getActorInventory world =
    maybeToEither "Active actor not found" $ do
        actor <- Map.lookup (activeActorId world) (actors world)
        return (actorLocation actor)

getContainerInventory :: Item -> Maybe Location
getContainerInventory = getInventory

getActorInventoryItems :: World -> [Item]
getActorInventoryItems gw = do
  let pocketSlot = getActorInventory gw
  getItemsAtLoc pocketSlot gw

findItemByTagInActorInventory :: Text -> World -> Maybe Item
findItemByTagInActorInventory iTag gw = do
  List.find (\e -> getTag e == iTag) (getActorInventoryItems gw)

checkItemTagInPocket :: Text -> World -> Bool
checkItemTagInPocket itemTag gw = do
  case findItemByTag itemTag gw of
    Just item -> getLocation item == getActorInventory gw
    Nothing   -> False

-- | Find any location by tag, checking both room locations and inventory locations
findAnyLocationByTag :: Text -> World -> Maybe Location
findAnyLocationByTag targetTag gw =
    -- First check room locations
    case List.find (\loc -> locTag loc == targetTag) (gwLocations gw) of
        Just loc -> Just loc
        Nothing ->
            -- Then check inventory locations
            let allEntities = getEntities gw
                inventoryLocs = mapMaybe getInventory allEntities
            in List.find (\loc -> locTag loc == targetTag) inventoryLocs

-- | Find a room location by tag (only searches gwLocations)
findRoomByTag :: Text -> World -> Maybe Location
findRoomByTag targetTag gw =
   List.find (\loc -> locTag loc == targetTag) (gwLocations gw)

-- | Get the inventory location of an entity if it exists
getEntityInventoryLoc :: Tagged a => a -> Maybe Location
getEntityInventoryLoc = getInventory

-- | Find a container's inventory location by the container's tag
findContainerLocationByTag :: Text -> World -> Maybe Location
findContainerLocationByTag containerTag gw = do
    container <- findItemByTag containerTag gw
    getInventory container

-- Helper to get objects at a location
getItemTagsAtLoc :: Location -> World -> [Text]
getItemTagsAtLoc loc gw = map getTag $ getItemsAtLoc loc gw

getEntitiesAtLoc :: Location -> World -> [Entity]
getEntitiesAtLoc loc gw =
    filter (\entity -> getLocation entity == loc) (getEntities gw)

getItemsAtLoc :: EntityId -> World -> [Entity 'ItemT]
getItemsAtLoc locId world =
    Map.elems $ Map.filter (\item -> getLocation item == locId) (items world)

findEntityByTagAtLoc :: Text -> Location -> World -> Maybe Entity
findEntityByTagAtLoc eTag loc gw =
    List.find (\e -> getTag e == eTag && getLocation e == loc) (getEntities gw)

findItemByTag :: Text -> World -> Either Text (Entity 'ItemT)
findItemByTag tag world =
    maybeToEither ("Item not found: " <> tag) $
        Map.find (\item -> getTag item == tag) (items world)

moveItemLoc :: Entity 'ItemT -> EntityId -> World -> World
moveItemLoc item newLocId = updateLocation newLocId item

updateItem :: (Item -> Item) -> Item -> World -> World
updateItem updateFn targetItem gameWorld =
    gameWorld { gwItems = updatedItems }
  where
    updatedItems = Prelude.map updateIfMatch (gwItems gameWorld)
    updateIfMatch item
      | item == targetItem = updateFn item
      | otherwise = item

isContainer :: Entity -> Bool
isContainer = isJust . getInventory

-- | Get the contents of a container
getContainerContents :: Entity -> World -> [Item]
getContainerContents container gw =
    case getInventory container of
        Nothing           -> []  -- Not a container
        Just containerLoc -> getItemsAtLoc containerLoc gw

-- | Get the container that holds an item (if any)
getItemContainer :: Item -> World -> Maybe Item
getItemContainer item gw = do
    let itemLoc = getLocation item
    List.find (\e -> isContainer e &&
               (getInventory e == Just itemLoc))
         (gwItems gw)

-- | Check if an item is inside a specific container
isInContainer :: Item -> Item -> Bool
isInContainer item container =
    case getInventory container of
        Nothing           -> False  -- Not a container
        Just containerLoc -> getLocation item == containerLoc

-- | Move an item into a container
moveItemToContainer :: Item -> Item -> World -> Either Text World
moveItemToContainer item container gw =
    case getInventory container of
        Nothing           -> Left $ "The " <> getName container <> " is not a container."
        Just containerLoc -> Right $ moveItemLoc item containerLoc gw

-- | Take an item out of a container and put it in a location
moveItemFromContainer :: Item -> Location -> World -> World
moveItemFromContainer = moveItemLoc

tagInItemList :: Text -> [Item] -> Bool
tagInItemList searchTag = any (\item -> getTag item == searchTag)

getAllVisibleItemsByTag :: Text -> World -> Bool -> [Item]
getAllVisibleItemsByTag targetTag gw checkContainers =
    let currentLoc = getActiveActorLoc gw
        -- Get items in current location (optionally checking containers)
        roomItems = findItemsByTagAtLoc targetTag currentLoc gw checkContainers
        -- Get items in inventory (optionally checking carried containers)
        inventoryItems = case findItemInInventory targetTag gw checkContainers of
            Nothing   -> []
            Just item -> [item]
    in nub $ roomItems ++ inventoryItems

getAllVisibleItems :: World -> [Item]
getAllVisibleItems gw =
    let roomItems = getItemsAtLoc (getActiveActorLoc gw) gw
        inventoryItems = getActorInventoryItems gw
    in roomItems ++ inventoryItems

-- | Get all items at a specific location, optionally including items in containers
getItemsAtLocDeep :: Location -> World -> Bool -> [Item]
getItemsAtLocDeep loc gw includeContainers =
    let directItems = getItemsAtLoc loc gw
        containerItems = if includeContainers
                         then concatMap (`getContainerContents` gw)
                              (filter isContainer directItems)
                         else []
        inventoryItems = getActorInventoryItems gw
    in directItems ++ containerItems ++ inventoryItems

-- | Find all items matching a specific tag at a location and its containers
findItemsByTagAtLoc :: Text -> Location -> World -> Bool -> [Item]
findItemsByTagAtLoc targetTag loc gw checkContainers =
    filter (\item -> getTag item == targetTag)
          (getItemsAtLocDeep loc gw checkContainers)

-- | Get all container items at a location
getContainersAtLoc :: Location -> World -> [Item]
getContainersAtLoc loc gw =
    filter isContainer (getItemsAtLoc loc gw)

-- | Get all non-container items at a location
getNonContainersAtLoc :: Location -> World -> [Item]
getNonContainersAtLoc loc gw =
    filter (not . isContainer) (getItemsAtLoc loc gw)

-- | Find items recursively in all containers at a location
findItemsInContainers :: Location -> World -> [Item]
findItemsInContainers loc gw =
    let containers = getContainersAtLoc loc gw
    in concatMap (`getContainerContents` gw) containers

-- | Get all items in the actor's inventory, optionally including items in carried containers
getInventoryItems :: World -> Bool -> [Item]
getInventoryItems gw includeContainers =
    let inv = getActorInventory gw
        directItems = getItemsAtLoc inv gw
        containerItems = if includeContainers
                        then concatMap (`getContainerContents` gw)
                             (filter isContainer directItems)
                        else []
    in directItems ++ containerItems

-- | Find an item by tag in the actor's inventory or carried containers
findItemInInventory :: Text -> World -> Bool -> Maybe Item
findItemInInventory targetTag gw includeContainers =
    List.find (\item -> getTag item == targetTag)
             (getInventoryItems gw includeContainers)

-- | Check if an item exists at a location (including in containers if specified)
itemExistsAtLoc :: Text -> Location -> World -> Bool -> Bool
itemExistsAtLoc targetTag loc gw checkContainers =
    not . null $ findItemsByTagAtLoc targetTag loc gw checkContainers

-- | Get all accessible items (in current location and inventory)
getAllAccessibleItems :: World -> Bool -> [Item]
getAllAccessibleItems gw includeContainers =
    let roomItems = getItemsAtLocDeep (getActiveActorLoc gw) gw includeContainers
        inventoryItems = getInventoryItems gw includeContainers
    in nub $ roomItems ++ inventoryItems  -- Remove duplicates

-- | Get formatted list of item names at a location
getItemNamesAtLoc :: Location -> World -> [Text]
getItemNamesAtLoc loc gw =
    map getName (getItemsAtLoc loc gw)

-- | Helper function to remove duplicates while preserving order
nub :: Eq a => [a] -> [a]
nub = List.foldl' (\seen x -> if x `elem` seen then seen else x : seen) []
