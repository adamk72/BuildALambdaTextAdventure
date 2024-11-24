{-# LANGUAGE GADTs #-}
module Entity.Ops.Inventory (module Entity.Ops.Inventory) where

import Entity.Entity
import Data.Map as Map

getEntityInventoryList :: Entity a -> World -> [Entity 'ItemT]
getEntityInventoryList entity world =
    let maybeContainerId = case entity of
            Location base _ -> Just $ entityId base
            Actor _ _ invBase -> Just $ entityId invBase
            Item _ _ (Just invBase) -> Just $ entityId invBase
            Item {} -> Nothing
    in case maybeContainerId of
        Nothing -> []
        Just containerId ->
            Prelude.filter (\item -> getLocationId item == containerId) (Map.elems $ items world)

getActiveActorInventoryList :: World -> [Entity 'ItemT]
getActiveActorInventoryList w = getEntityInventoryList (activeActor w) w