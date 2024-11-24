module Entity.Ops.Inventory (module Entity.Ops.Inventory) where

import           Entity.Class.Capacity (getItemList)
import Entity.Entity
import Entity.Class.EntityBase

-- Todo: clean this up to use only capacity functions; here because of backwards compatibility.
getEntityInventoryList :: HasEntityBase a => Entity a -> World -> [Entity 'ItemT]
getEntityInventoryList e = getItemList (getId e)

getActiveActorInventoryList :: World -> [Entity 'ItemT]
getActiveActorInventoryList w = getEntityInventoryList (activeActor w) w
