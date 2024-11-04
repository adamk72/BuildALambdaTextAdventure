module Core.State.Operations (module Core.State.Operations) where

import           Core.State.Entity
import           Core.State.GameState    (GameWorld (..))
import           Core.State.Location     (Location)
import           Core.State.TaggedEntity

setEntityLoc :: Location -> Entity -> Entity
setEntityLoc newLoc entity =
    entity { entityTag = (entityTag entity) { location = newLoc } }

setCharLoc :: Location -> Character -> Character
setCharLoc = setEntityLoc

getActiveEntityLocFromGW :: (GameWorld -> Entity) -> GameWorld -> Location
getActiveEntityLocFromGW ae gw = location $ entityTag $ ae gw

getActiveCharLoc :: GameWorld -> Location
getActiveCharLoc = getActiveEntityLocFromGW  gwActiveCharacter

updateInteractable :: (Interactable -> Interactable) -> Interactable -> GameWorld -> GameWorld
updateInteractable updateFn targetItem gameWorld =
    gameWorld { gwInteractables = updatedInteractables }
  where
    updatedInteractables = Prelude.map updateIfMatch (gwInteractables gameWorld)
    updateIfMatch item
      | item == targetItem = updateFn item
      | otherwise = item