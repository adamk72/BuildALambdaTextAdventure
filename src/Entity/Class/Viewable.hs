{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}

module Entity.Class.Viewable (ViewablesRecord (..), getLocationId, getViewablesRecordByLocId) where

import qualified Data.Map            as Map
import           Entity.Entity
import           Entity.Types.Common

class Viewable (a :: EntityType) where
    getLocationId :: Entity a -> LocationId

instance Viewable 'ActorT where
    getLocationId (Actor _ loc _) = loc

instance Viewable 'ItemT where
    getLocationId (Item _ loc _) = loc

data ViewablesRecord = ViewablesRecord
    { viewableItems  :: [Entity 'ItemT]
    , viewableActors :: [Entity 'ActorT]
    }

getViewablesRecordByLocId :: LocationId -> World -> ViewablesRecord
getViewablesRecordByLocId locId world = ViewablesRecord
    { viewableItems = Map.elems $ Map.filter (\item -> getLocationId item == locId) (items world)
    , viewableActors = Map.elems $ Map.filter (\actor -> getLocationId actor == locId) (actors world)
    }
