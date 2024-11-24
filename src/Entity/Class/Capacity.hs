{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module Entity.Class.Capacity (module Entity.Class.Capacity) where

import qualified Data.Map                as Map
import           Data.Text               (Text)
import           Entity.Class.EntityBase (HasEntityBase, getId)
import           Entity.Entity
    (Entity (Actor, Item, Location, actorBase, itemBase, itemLocationId, locationBase), EntityBase (entityId), EntityId,
    EntityType (..), World (items))
import           Entity.Types            (Capacity (..))

class HasCapacity (a :: EntityType) where
    getCapacity :: Entity a -> Capacity
    getInventoryCount :: Entity a -> World -> Int
    canAddItem :: Entity a -> World -> Bool

instance HasCapacity 'LocationT where
    getCapacity (Location {}) = Unlimited
    getInventoryCount loc world =
        length $ getItemList (entityId $ locationBase loc) world
    canAddItem _ _ = True  -- Locations can always accept items

instance HasCapacity 'ActorT where
    getCapacity (Actor _ _ cap) = cap -- Example: Limited 10 ≈ actors can carry 10 items
    getInventoryCount actor world =
        length $ getItemList (entityId $ actorBase actor) world
    canAddItem actor world =
        case getCapacity actor of
            Limited n -> getInventoryCount actor world < n
            Unlimited -> True
            None      -> False

instance HasCapacity 'ItemT where
    getCapacity (Item _ _ Nothing)    = None
    getCapacity (Item _ _ (Just cap)) = cap
    getInventoryCount item world =
        length $ getItemList (entityId $ itemBase item) world
    canAddItem item world =
        case getCapacity item of
            Limited n -> getInventoryCount item world < n
            Unlimited -> True
            None      -> False

getItemList :: EntityId -> World -> [Entity 'ItemT]
getItemList containerId world =
    Map.elems $ Map.filter (\item -> itemLocationId item == containerId) (items world)

addItem :: (HasCapacity a, HasEntityBase a) => Entity a -> Entity 'ItemT -> World -> Either Text World
addItem container item world =
    if canAddItem container world
    then Right $ updateItemLocation (entityId $ itemBase item) (getId container) world
    else Left "Container is full"

updateItemLocation :: EntityId -> EntityId -> World -> World
updateItemLocation itemId newLoc world =
    world { items = Map.adjust (\item -> item { itemLocationId = newLoc }) itemId (items world) }
