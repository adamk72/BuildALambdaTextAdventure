module Core.State.Operations (module Core.State.Operations) where

import           Core.State.Entity
import           Core.State.EntityContainer
import           Core.State.GameState       (GameWorld (..))
import           Core.State.Location        (Location)
import           Core.State.TaggedEntity
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
getActorInventory gw = do
  let ac = gwActiveActor gw
  fromJust (getInventory ac) -- guaranteed to be Just because it's a character

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
isInContainer :: Item -> Item -> GameWorld -> Bool
isInContainer item container gw =
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
