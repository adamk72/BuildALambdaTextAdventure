module Core.State.Operations (module Core.State.Operations) where

import           Core.State.Entity
import           Core.State.GameState    (GameWorld (..))
import           Core.State.Location     (Location)
import           Core.State.TaggedEntity
import qualified Data.List               as List
import           Data.Maybe              (fromJust)
import           Data.Text               (Text)

setEntityLoc :: Location -> Entity -> Entity
setEntityLoc newLoc entity =
    entity { entityTag = (entityTag entity) { location = newLoc } }

setActorLoc :: Location -> Actor -> Actor
setActorLoc = setEntityLoc

getActiveActorLoc :: GameWorld -> Location
getActiveActorLoc gw = location $ entityTag $ gwActiveActor gw

getPocketSlot :: GameWorld -> Location
getPocketSlot gw = do
  let ac = gwActiveActor gw
      pocketSlot = findLocInInventoryByTag (getTag ac) ac
  fromJust pocketSlot -- guaranteed to be Just because it's a character

checkItemTagInPocket :: Text -> Actor -> Bool
checkItemTagInPocket itemTag actor = if findLocInInventoryByTag itemTag actor == Nothing then False else True

-- Helper to get objects at a location
getItemsAtLoc :: Location -> GameWorld -> [Item]
getItemsAtLoc loc gw =
    filter (\item -> getLocation item == loc) (gwItems gw)

findItemByTag :: Text -> GameWorld -> Maybe Item
findItemByTag itemTag gw = List.find (\item -> getTag item == itemTag) (gwItems gw)

updateItem :: (Item -> Item) -> Item -> GameWorld -> GameWorld
updateItem updateFn targetItem gameWorld =
    gameWorld { gwItems = updatedItems }
  where
    updatedItems = Prelude.map updateIfMatch (gwItems gameWorld)
    updateIfMatch item
      | item == targetItem = updateFn item
      | otherwise = item
