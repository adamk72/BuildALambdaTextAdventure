{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}

module Entity.Class.Movable (module Entity.Class.Movable) where

import           Entity.Entity
import           Entity.Types.Common

class Movable (a :: EntityType) where
    getLocationId :: Entity a -> LocationId
    setLocationId :: LocationId -> Entity a -> Entity a

instance Movable 'ActorT where
    getLocationId (Actor _ loc _) = loc
    setLocationId newLoc (Actor base _ inv) = Actor base newLoc inv

data ViewablesRecord = ViewablesRecord
    { viewableItems  :: [Entity 'ItemT]
    , viewableActors :: [Entity 'ActorT]
    }
