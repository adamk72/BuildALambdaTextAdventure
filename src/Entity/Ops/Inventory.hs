{-# LANGUAGE GADTs #-}
module Entity.Ops.Inventory (module Entity.Ops.Inventory) where

import Entity.Entity
import Data.Text (Text)
import Data.Map as Map
import Data.Maybe (isJust)
import Entity.Ops.Location

-- | Types and basic definitions
data InventoryError
    = NotAContainerTarget Text       -- ^ Target is not a container
    | InvalidContainer Text          -- ^ Container doesn't exist
    | InvalidItemTag Text            -- ^ Item doesn't exist
    | ItemNotVisible Text Text       -- ^ Item not in current location (includes item tag and location)
    | SameContainer Text             -- ^ Item is already in this container
    deriving (Show, Eq)

type InventoryResult = Either InventoryError World

-- | Core inventory access
getActiveActorInventoryID :: World -> InventoryId
getActiveActorInventoryID w = entityId (actorInventory (activeActor w))

getInventory :: Entity a -> Maybe (EntityBase 'LocationT)
getInventory entity = case entity of
    Location {} -> Nothing
    Actor {actorInventory = inv} -> Just inv
    Item {itemInventory = invM} -> invM

hasInventory :: Entity a -> Bool
hasInventory = isJust . getInventory


getInventoryId :: Entity a -> Maybe InventoryId
getInventoryId entity = entityId <$> getInventory entity

-- | Inventory creation
createActorInventory :: LocationId -> Text -> EntityBase 'LocationT
createActorInventory ownerId name = EntityBase
    { entityId = EntityId (unEntityId ownerId <> "-inventory")
    , entityTags = Nothing
    , entityName = name <> "'s inventory"
    }

createItemInventory :: LocationId -> Text -> EntityBase 'LocationT
createItemInventory containerId name = EntityBase
    { entityId = EntityId (unEntityId containerId <> "-contents")
    , entityTags = Nothing
    , entityName = "inside " <> name
    }

-- | Inventory modification
setActorInventory :: EntityBase 'LocationT -> Entity 'ActorT -> Entity 'ActorT
setActorInventory newInv actor = actor { actorInventory = newInv }

setItemInventory :: Maybe (EntityBase 'LocationT) -> Entity 'ItemT -> Entity 'ItemT
setItemInventory newInv item = item { itemInventory = newInv }

updateActorInventory :: ActorId -> EntityBase 'LocationT -> World -> World
updateActorInventory actorId newInv world =
    world { actors = Map.adjust (setActorInventory newInv) actorId (actors world) }

updateItemInventory :: ItemId -> Maybe (EntityBase 'LocationT) -> World -> World
updateItemInventory itemId newInv world =
    world { items = Map.adjust (setItemInventory newInv) itemId (items world) }

makeItemContainer :: Entity 'ItemT -> World -> (Entity 'ItemT, World)
makeItemContainer item world =
    let newInv = createItemInventory (getId item) (getName item)
        updatedItem = setItemInventory (Just newInv) item
    in (updatedItem, world { items = Map.insert (getId item) updatedItem (items world) })

makeActorContainer :: Entity 'ActorT -> World -> (Entity 'ActorT, World)
makeActorContainer actor world =
    let newInv = createActorInventory (getId actor) (getName actor)
        updatedActor = setActorInventory newInv actor
    in (updatedActor, world { actors = Map.insert (getId actor) updatedActor (actors world) })

-- | LEGACY OPERATIONS
-- | For supporting refactor process


isInInventoryOf :: (Movable a) => Entity a -> Entity b -> Bool
isInInventoryOf movable container =
    case getInventoryId container of
        Just invId -> getLocationId movable == invId
        Nothing -> False

getActiveActorInventoryList :: World -> [Entity 'ItemT]
getActiveActorInventoryList w = getEntityInventoryList (activeActor w) w

getEntityInventoryList :: Entity a -> World -> [Entity 'ItemT]
getEntityInventoryList entity world =
    case getInventoryId entity of
        Nothing -> []
        Just invId ->
            Prelude.filter (\item -> getLocationId item == invId) $
            Map.elems $ items world

-- | Movement operations
moveItemToContainer :: HasEntityBase a => Entity 'ItemT -> Entity a -> Entity 'LocationT -> World -> InventoryResult
moveItemToContainer item target currentLoc world =
    case getInventory target of
        Nothing ->
            Left $ NotAContainerTarget (getName target)
        Just containerInv -> do
            let containerId = entityId containerInv
                itemName = getName item

            if getLocationId item == containerId
                then Left $ SameContainer itemName
                else Right $ updateLocation containerId item world -- Todo: Check for visibility later
