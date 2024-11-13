module Entity.Inventory where

import Entity.Entity
import Data.Text (Text)
import Data.Map as Map
import Data.Maybe (isJust)

-- | Types and basic definitions
data InventoryError
    = NotAContainer Text              -- ^ Target is not a container
    | InvalidContainer Text           -- ^ Container doesn't exist
    | InvalidItem Text               -- ^ Item doesn't exist
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
    Item {itemInventory = invM} - invM

hasInventory :: Entity a -> Bool
hasInventory = isJust . getInventory

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

-- | Query operations
isInInventoryOf :: (Movable a) => Entity a -> Entity b -> Bool
isInInventoryOf movable container =
    case getInventoryId container of
        Just invId -> getLocation movable == invId
        Nothing -> False

getInventoryContents :: Entity a -> World -> [Entity 'ItemT]
getInventoryContents entity world =
    case getInventoryId entity of
        Nothing -> []
        Just invId ->
            filter (\item -> getLocation item == invId) $
            Map.elems $ items world

-- | Visibility checking
isItemVisible :: Entity 'ItemT -> Entity 'LocationT -> World -> Bool
isItemVisible item loc world =
    let locId = getId loc
        directlyVisible = getLocation item == locId
        visibleInContainers = any (isInInventoryOf item) (getContainersAtLoc locId world)
    in directlyVisible || visibleInContainers

getContainersAtLoc :: EntityId -> World -> [Entity a]
getContainersAtLoc locId world =
    let itemContainers = filter hasInventory $
            Map.elems $ Map.filter (\i -> getLocation i == locId) (items world)
        actorContainers =
            Map.elems $ Map.filter (\a -> getLocation a == locId) (actors world)
    in itemContainers ++ actorContainers

-- | Movement operations
moveItemToContainer :: Entity 'ItemT -> Entity a -> Entity 'LocationT -> World -> InventoryResult
moveItemToContainer item target currentLoc world =
    case getInventory target of
        Nothing ->
            Left $ NotAContainer (getName target)
        Just containerInv -> do
            let containerId = entityId containerInv
                itemName = getName item

            if getLocation item == containerId
                then Left $ SameContainer itemName
                else if isItemVisible item currentLoc world
                    then Right $ updateLocation containerId item world
                    else Left $ ItemNotVisible itemName (getName currentLoc)

tryMoveItem :: Text -> Text -> World -> InventoryResult
tryMoveItem itemTag containerTag world =
    case (findEntityById itemTag world, findEntityById containerTag world) of
        (Just item@Item{}, Just container) -> do
            case Map.lookup (getLocation item) (locations world) of
                Just loc -> moveItemToContainer item container loc world
                Nothing -> error "Invalid item location" -- Should never happen
        (Nothing, _) -> Left $ InvalidItem itemTag
        (_, Nothing) -> Left $ InvalidContainer containerTag
        _ -> Left $ InvalidItem itemTag