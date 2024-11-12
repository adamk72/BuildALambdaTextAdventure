module Core.State.EntityContainer (module Core.State.EntityContainer) where

import           Entity.Entity
import           Core.State.GameState

class EntityContainer c where
    getEntities :: c -> [Entity a]

instance EntityContainer World where
    getEntities gw = undefined -- gwItems gw ++ gwPlayableActors gw
