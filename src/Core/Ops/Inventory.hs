{-# LANGUAGE GADTs #-}
module Core.Ops.Inventory (module Core.Ops.Inventory) where

import Entity.Entity
import Core.Ops.EntityInfo
import Data.Text (Text)
import Data.Map as Map
import Data.Maybe (isJust)
import Core.Ops.Location

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
getActiveActorInventoryID :: World -> EntityId
getActiveActorInventoryID w = entityId (actorInventory (activeActor w))

getInventory :: Entity a -> Maybe (EntityBase 'LocationT)
getInventory entity = case entity of
    Location {} -> Nothing
    Actor {actorInventory = inv} -> Just inv
    Item {itemInventory = invM} -> invM

hasInventory :: Entity a -> Bool
hasInventory = isJust . getInventory

-- | Helper to check if a SomeEntity has inventory
hasSomeInventory :: SomeEntity -> Bool
hasSomeInventory (SomeEntity e) = hasInventory e

-- | Helper to get inventory from a SomeEntity
getSomeInventory :: SomeEntity -> Maybe (EntityBase 'LocationT)
getSomeInventory (SomeEntity e) = getInventory e

getInventoryId :: Entity a -> Maybe EntityId
getInventoryId entity = entityId <$> getInventory entity

-- | Inventory creation
createActorInventory :: EntityId -> Text -> EntityBase 'LocationT
createActorInventory ownerId name = EntityBase
    { entityId = EntityId (unEntityId ownerId <> "-inventory")
    , entityTags = Nothing
    , entityName = name <> "'s inventory"
    }

createItemInventory :: EntityId -> Text -> EntityBase 'LocationT
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

updateActorInventory :: EntityId -> EntityBase 'LocationT -> World -> World
updateActorInventory actorId newInv world =
    world { actors = Map.adjust (setActorInventory newInv) actorId (actors world) }

updateItemInventory :: EntityId -> Maybe (EntityBase 'LocationT) -> World -> World
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

checkItemTagInPocket :: Text -> World -> Bool
checkItemTagInPocket itemTag gw = do
  let actorInventoryList = getActiveActorInventoryList gw
  case findEntityByTag itemTag gw of
    Just (SomeEntity someItem) -> do
        case someItem of
            thing@Item{} -> thing `elem` actorInventoryList
            _ -> False
    Nothing -> False

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

-- | Visibility checking
isItemVisible :: Entity 'ItemT -> Entity 'LocationT -> World -> Bool
isItemVisible item loc world =
    let locId = getId loc
        directlyVisible = getLocationId item == locId
        visibleInContainers = Prelude.any visibleInContainer (getContainersAtLoc locId world)
    in directlyVisible || visibleInContainers
    where
        visibleInContainer :: SomeEntity -> Bool
        visibleInContainer (SomeEntity container) = isInInventoryOf item container

getContainersAtLoc :: EntityId -> World -> [SomeEntity]
getContainersAtLoc locId world =
    let itemContainers = Prelude.map SomeEntity $
            Prelude.filter hasInventory $
            Map.elems $ Map.filter (\i -> getLocationId i == locId) (items world)
        actorContainers = Prelude.map SomeEntity $
            Map.elems $ Map.filter (\a -> getLocationId a == locId) (actors world)
    in itemContainers ++ actorContainers

-- | Movement operations
moveItemToContainer :: Tagged a => Entity 'ItemT -> Entity a -> Entity 'LocationT -> World -> InventoryResult
moveItemToContainer item target currentLoc world =
    case getInventory target of
        Nothing ->
            Left $ NotAContainerTarget (getName target)
        Just containerInv -> do
            let containerId = entityId containerInv
                itemName = getName item

            if getLocationId item == containerId
                then Left $ SameContainer itemName
                else if isItemVisible item currentLoc world
                    then Right $ updateLocation containerId item world
                    else Left $ ItemNotVisible itemName (getName currentLoc)

-- Todo: Clean this up later; this is a work around for a problem with Tagged.
tryMoveItem :: Text -> Text -> World -> InventoryResult
tryMoveItem itemTag containerTag world =
    case (findEntityByTag itemTag world, findEntityByTag containerTag world) of
        (Just (SomeEntity item@Item{}), Just (SomeEntity container)) ->
            case container of
                Location {} ->
                    case Map.lookup (getLocationId item) (locations world) of
                        Just loc -> moveItemToContainer item container loc world
                        Nothing -> error "Invalid item location"
                Actor {} ->
                    case Map.lookup (getLocationId item) (locations world) of
                        Just loc -> moveItemToContainer item container loc world
                        Nothing -> error "Invalid item location"
                Item {} ->
                    case Map.lookup (getLocationId item) (locations world) of
                        Just loc -> moveItemToContainer item container loc world
                        Nothing -> error "Invalid item location"
        (Nothing, _) -> Left $ InvalidItemTag itemTag
        (_, Nothing) -> Left $ InvalidContainer containerTag
        _ -> Left $ InvalidItemTag itemTag