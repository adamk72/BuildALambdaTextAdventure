module Core.State.Operations (module Core.State.Operations) where

import           Core.State.Entity
import           Core.State.EntityContainer
import           Core.State.GameState    (GameWorld (..))
import           Core.State.Location     (Location)
import           Core.State.TaggedEntity
import qualified Data.List               as List
import           Data.Maybe              (fromJust, isJust)
import           Data.Text               (Text)

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
  fromJust (getInventory ac)-- guaranteed to be Just because it's a character

getActorInventory :: GameWorld -> [Item]
getActorInventory gw = do
  let ps = getActorInventory gw
  getItemsAtLoc ps gw

checkItemTagInPocket :: Text -> Actor -> Bool
checkItemTagInPocket itemTag actor = isJust (findLocInInventoryByTag itemTag actor)

-- Helper to get objects at a location
getEntitiesAtLoc :: Location -> GameWorld -> [Entity]
getEntitiesAtLoc loc gw =
    filter (\entity -> getLocation entity == loc) (getEntities gw)

getItemsAtLoc :: Location -> GameWorld -> [Item]
getItemsAtLoc loc gw = filterItems (getEntitiesAtLoc loc gw)

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
