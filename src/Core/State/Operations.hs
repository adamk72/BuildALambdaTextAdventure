module Core.State.Operations (module Core.State.Operations) where

import           Entity.Entity
import           Core.State.EntityContainer
import           Core.State.GameState       (GameWorld (..))
import           Core.State.Location        (Location (..))
import qualified Data.List                  as List
import           Data.Maybe
import           Data.Text                  (Text)

setEntityLoc :: Location -> Entity -> Entity
setEntityLoc newLoc entity =
    entity { entityTag = (entityTag entity) { location = newLoc } }

setActorLoc :: Location -> Actor -> Actor
setActorLoc = setEntityLoc

getActiveActorLoc :: GameWorld -> Location
getActiveActorLoc gw = location $ entityTag $ gwActiveActor gw

getActorInventory :: GameWorld -> Location
getActorInventory gw = fromJust (getInventory (gwActiveActor gw))
    -- Safe to use fromJust here as actors always have inventories

getContainerInventory :: Item -> Maybe Location
getContainerInventory = getInventory

getActorInventoryItems :: GameWorld -> [Item]
getActorInventoryItems gw = do
  let pocketSlot = getActorInventory gw
  getItemsAtLoc pocketSlot gw

findItemByTagInActorInventory :: Text -> GameWorld -> Maybe Item
findItemByTagInActorInventory iTag gw = do
  List.find (\e -> getTag e == iTag) (getActorInventoryItems gw)

checkItemTagInPocket :: Text -> GameWorld -> Bool
checkItemTagInPocket itemTag gw = do
  case findItemByTag itemTag gw of
    Just item -> getLocation item == getActorInventory gw
    Nothing   -> False

-- | Find any location by tag, checking both room locations and inventory locations
findAnyLocationByTag :: Text -> GameWorld -> Maybe Location
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
findRoomByTag :: Text -> GameWorld -> Maybe Location
findRoomByTag targetTag gw =
   List.find (\loc -> locTag loc == targetTag) (gwLocations gw)

-- | Get the inventory location of an entity if it exists
getEntityInventoryLoc :: Tagged a => a -> Maybe Location
getEntityInventoryLoc = getInventory

-- | Find a container's inventory location by the container's tag
findContainerLocationByTag :: Text -> GameWorld -> Maybe Location
findContainerLocationByTag containerTag gw = do
    container <- findItemByTag containerTag gw
    getInventory container

-- Helper to get objects at a location
getItemTagsAtLoc :: Location -> GameWorld -> [Text]
getItemTagsAtLoc loc gw = map getTag $ getItemsAtLoc loc gw

getEntitiesAtLoc :: Location -> GameWorld -> [Entity]
getEntitiesAtLoc loc gw =
    filter (\entity -> getLocation entity == loc) (getEntities gw)

getItemsAtLoc :: Location -> GameWorld -> [Item]
getItemsAtLoc loc gw = filterItems (getEntitiesAtLoc loc gw)

findEntityByTagAtLoc :: Text -> Location -> GameWorld -> Maybe Entity
findEntityByTagAtLoc eTag loc gw =
    List.find (\e -> getTag e == eTag && getLocation e == loc) (getEntities gw)

findItemByTag :: Text -> GameWorld -> Maybe Item
findItemByTag itemTag gw = List.find (\item -> getTag item == itemTag) (gwItems gw)

moveItemLoc :: Item -> Location -> GameWorld -> GameWorld
moveItemLoc itemToMove loc = updateItem (\item -> item { entityTag = (entityTag item) { location = loc } }) itemToMove

updateItem :: (Item -> Item) -> Item -> GameWorld -> GameWorld
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
getContainerContents :: Entity -> GameWorld -> [Item]
getContainerContents container gw =
    case getInventory container of
        Nothing           -> []  -- Not a container
        Just containerLoc -> getItemsAtLoc containerLoc gw

-- | Get the container that holds an item (if any)
getItemContainer :: Item -> GameWorld -> Maybe Item
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
moveItemToContainer :: Item -> Item -> GameWorld -> Either Text GameWorld
moveItemToContainer item container gw =
    case getInventory container of
        Nothing           -> Left $ "The " <> getName container <> " is not a container."
        Just containerLoc -> Right $ moveItemLoc item containerLoc gw

-- | Take an item out of a container and put it in a location
moveItemFromContainer :: Item -> Location -> GameWorld -> GameWorld
moveItemFromContainer = moveItemLoc

tagInItemList :: Text -> [Item] -> Bool
tagInItemList searchTag = any (\item -> getTag item == searchTag)

getAllVisibleItemsByTag :: Text -> GameWorld -> Bool -> [Item]
getAllVisibleItemsByTag targetTag gw checkContainers =
    let currentLoc = getActiveActorLoc gw
        -- Get items in current location (optionally checking containers)
        roomItems = findItemsByTagAtLoc targetTag currentLoc gw checkContainers
        -- Get items in inventory (optionally checking carried containers)
        inventoryItems = case findItemInInventory targetTag gw checkContainers of
            Nothing   -> []
            Just item -> [item]
    in nub $ roomItems ++ inventoryItems

getAllVisibleItems :: GameWorld -> [Item]
getAllVisibleItems gw =
    let roomItems = getItemsAtLoc (getActiveActorLoc gw) gw
        inventoryItems = getActorInventoryItems gw
    in roomItems ++ inventoryItems

-- | Get all items at a specific location, optionally including items in containers
getItemsAtLocDeep :: Location -> GameWorld -> Bool -> [Item]
getItemsAtLocDeep loc gw includeContainers =
    let directItems = getItemsAtLoc loc gw
        containerItems = if includeContainers
                         then concatMap (`getContainerContents` gw)
                              (filter isContainer directItems)
                         else []
        inventoryItems = getActorInventoryItems gw
    in directItems ++ containerItems ++ inventoryItems

-- | Find all items matching a specific tag at a location and its containers
findItemsByTagAtLoc :: Text -> Location -> GameWorld -> Bool -> [Item]
findItemsByTagAtLoc targetTag loc gw checkContainers =
    filter (\item -> getTag item == targetTag)
          (getItemsAtLocDeep loc gw checkContainers)

-- | Get all container items at a location
getContainersAtLoc :: Location -> GameWorld -> [Item]
getContainersAtLoc loc gw =
    filter isContainer (getItemsAtLoc loc gw)

-- | Get all non-container items at a location
getNonContainersAtLoc :: Location -> GameWorld -> [Item]
getNonContainersAtLoc loc gw =
    filter (not . isContainer) (getItemsAtLoc loc gw)

-- | Find items recursively in all containers at a location
findItemsInContainers :: Location -> GameWorld -> [Item]
findItemsInContainers loc gw =
    let containers = getContainersAtLoc loc gw
    in concatMap (`getContainerContents` gw) containers

-- | Get all items in the actor's inventory, optionally including items in carried containers
getInventoryItems :: GameWorld -> Bool -> [Item]
getInventoryItems gw includeContainers =
    let inv = getActorInventory gw
        directItems = getItemsAtLoc inv gw
        containerItems = if includeContainers
                        then concatMap (`getContainerContents` gw)
                             (filter isContainer directItems)
                        else []
    in directItems ++ containerItems

-- | Find an item by tag in the actor's inventory or carried containers
findItemInInventory :: Text -> GameWorld -> Bool -> Maybe Item
findItemInInventory targetTag gw includeContainers =
    List.find (\item -> getTag item == targetTag)
             (getInventoryItems gw includeContainers)

-- | Check if an item exists at a location (including in containers if specified)
itemExistsAtLoc :: Text -> Location -> GameWorld -> Bool -> Bool
itemExistsAtLoc targetTag loc gw checkContainers =
    not . null $ findItemsByTagAtLoc targetTag loc gw checkContainers

-- | Get all accessible items (in current location and inventory)
getAllAccessibleItems :: GameWorld -> Bool -> [Item]
getAllAccessibleItems gw includeContainers =
    let roomItems = getItemsAtLocDeep (getActiveActorLoc gw) gw includeContainers
        inventoryItems = getInventoryItems gw includeContainers
    in nub $ roomItems ++ inventoryItems  -- Remove duplicates

-- | Get formatted list of item names at a location
getItemNamesAtLoc :: Location -> GameWorld -> [Text]
getItemNamesAtLoc loc gw =
    map getName (getItemsAtLoc loc gw)

-- | Helper function to remove duplicates while preserving order
nub :: Eq a => [a] -> [a]
nub = List.foldl' (\seen x -> if x `elem` seen then seen else x : seen) []
