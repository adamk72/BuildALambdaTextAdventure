{-# LANGUAGE PatternSynonyms #-}

module Core.State.Operations
    ( setEntityLoc
    , setCharLoc
    , getActiveEntityLocFromGW
    ) where

import Core.State.Entity
import Core.State.Location (Location)
import Core.State.GameState (GameWorld(..))
import Core.State.TaggedEntity (TaggedEntity(..))

setEntityLoc :: Location -> Entity -> Entity
setEntityLoc newLoc entity =
    entity { entityTag = (entityTag entity) { location = newLoc } }

setCharLoc :: Location -> Character -> Character
setCharLoc = setEntityLoc

getActiveEntityLocFromGW :: (GameWorld -> Entity) -> GameWorld -> Location
getActiveEntityLocFromGW ae gw = location $ entityTag $ ae gw