{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}

module Entity.Class.Conveyable (module Entity.Class.Conveyable) where

import           Entity.Entity

class Conveyable (a :: EntityType) where
    getLocationId :: Entity a -> LocationId
    setLocationId :: LocationId -> Entity a -> Entity a

instance Conveyable 'ItemT where
    getLocationId (Item _ loc _) = loc
    setLocationId newLoc (Item base _ inv) = Item base newLoc inv
