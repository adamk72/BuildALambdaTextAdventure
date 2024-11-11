module Core.State.EntityContainer (module Core.State.EntityContainer) where

import           Entity.Entity
import           Core.State.GameState

class EntityContainer a where
    getEntities :: a -> [Entity]

instance EntityContainer GameWorld where
    getEntities gw = gwItems gw ++ gwPlayableActors gw
