{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}

module Entity.Class.Viewable (module Entity.Class.Viewable) where

import           Entity.Entity

class Viewable (a :: EntityType) where
    getLocationId :: Entity a -> LocationId
    setLocationId :: LocationId -> Entity a -> Entity a

instance Viewable 'ActorT where
    getLocationId (Actor _ loc _) = loc
    setLocationId newLoc (Actor base _ inv) = Actor base newLoc inv

instance Viewable 'ItemT where
    getLocationId (Item _ loc _) = loc
    setLocationId newLoc (Item base _ inv) = Item base newLoc inv

data ViewablesRecord = ViewablesRecord
    { viewableItems  :: [Entity 'ItemT]
    , viewableActors :: [Entity 'ActorT]
    }
