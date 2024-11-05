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

getActiveActorLoc :: (GameWorld -> Actor) -> GameWorld -> Location
getActiveActorLoc ae gw = location $ entityTag $ ae gw

gwActiveActorLoc :: GameWorld -> Location
gwActiveActorLoc = getActiveActorLoc gwActiveActor

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
