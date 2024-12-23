module Entity.Ops.Inventory (getActiveActorInventoryList, getEntityInventoryList) where

import           Entity.Class.Capacity   (getItemList)
import           Entity.Class.EntityBase
import           Entity.Entity

-- Todo: clean this up to use only capacity functions; here because of backwards compatibility.
getEntityInventoryList :: (HasEntityBase a) => Entity a -> World -> [Entity 'ItemT]
getEntityInventoryList e = getItemList (getId e)

getActiveActorInventoryList :: World -> [Entity 'ItemT]
getActiveActorInventoryList w = getEntityInventoryList (activeActor w) w
