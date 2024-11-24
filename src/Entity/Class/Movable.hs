{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}

module Entity.Class.Movable (module Entity.Class.Movable) where

import           Entity.Entity

class Movable (a :: EntityType) where
    getLocationId :: Entity a -> MovableId
    setLocationId :: LocationId -> Entity a -> Entity a

instance Movable 'ActorT where
    getLocationId (Actor _ loc _) = loc
    setLocationId newLoc (Actor base _ inv) = Actor base newLoc inv

instance Movable 'ItemT where
    getLocationId (Item _ loc _) = loc
    setLocationId newLoc (Item base _ inv) = Item base newLoc inv

data MovablesRecord = MovablesRecord
    { movableItems  :: [Entity 'ItemT]
    , movableActors :: [Entity 'ActorT]
    }
