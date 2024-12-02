{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}

module Entity.Class.CanMoveSelf (module Entity.Class.CanMoveSelf) where

import           Entity.Entity
import           Entity.Types.Common

class CanMoveSelf (a :: EntityType) where
    getLocationId :: Entity a -> LocationId
    setLocationId :: LocationId -> Entity a -> Entity a

instance CanMoveSelf 'ActorT where
    getLocationId (Actor _ loc _) = loc
    setLocationId newLoc (Actor base _ inv) = Actor base newLoc inv
