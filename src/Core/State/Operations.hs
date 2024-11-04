module Core.State.Operations (module Core.State.Operations) where

import           Core.State.Entity
import           Core.State.GameState    (GameWorld (..))
import           Core.State.Location     (Location)
import           Core.State.TaggedEntity
import Data.Text (Text)
import qualified Data.List as List

setEntityLoc :: Location -> Entity -> Entity
setEntityLoc newLoc entity =
    entity { entityTag = (entityTag entity) { location = newLoc } }

setActorLoc :: Location -> Actor -> Actor
setActorLoc = setEntityLoc

getActiveEntityLocFromGW :: (GameWorld -> Entity) -> GameWorld -> Location
getActiveEntityLocFromGW ae gw = location $ entityTag $ ae gw

getActiveActorLoc :: GameWorld -> Location
getActiveActorLoc = getActiveEntityLocFromGW  getActiveActor

-- Helper to get objects at a location
getItemsAtLoc :: Location -> GameWorld -> [Item]
getItemsAtLoc loc gw =
    filter (\item -> getLocation item == loc) (gwItems gw)

findItemByLocTag :: Text -> GameWorld -> Maybe Item
findItemByLocTag itemTag gw = List.find (\item -> getTag item == itemTag) (gwItems gw)

updateItem :: (Item -> Item) -> Item -> GameWorld -> GameWorld
updateItem updateFn targetItem gameWorld =
    gameWorld { gwItems = updatedItems }
  where
    updatedItems = Prelude.map updateIfMatch (gwItems gameWorld)
    updateIfMatch item
      | item == targetItem = updateFn item
      | otherwise = item
