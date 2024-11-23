{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
module Entity.Entity (module Entity.Entity) where

import           Data.Map  as Map
import           Data.Text (Text)

-- Core types remain the same
data EntityType = LocationT | ActorT | ItemT

newtype EntityId = EntityId { unEntityId :: Text }
    deriving (Show, Eq, Ord)

type LocationId = EntityId
type ActorId = EntityId
type ItemId = EntityId
type InventoryId = EntityId

data EntityBase (a :: EntityType) = EntityBase
    { entityId   :: EntityId
    , entityTags :: Maybe [Text]
    , entityName :: Text
    } deriving (Show, Eq)

-- Entity GADT remains but we'll add helper functions for common operations
data Entity (a :: EntityType) where
    Location ::
        { locationBase   :: EntityBase 'LocationT
        , destinations   :: [LocationId]
        } -> Entity 'LocationT

    Actor ::
        { actorBase        :: EntityBase 'ActorT
        , actorLocationId  :: LocationId
        , actorInventory   :: EntityBase 'LocationT
        } -> Entity 'ActorT

    Item ::
        { itemBase        :: EntityBase 'ItemT
        , itemLocationId  :: LocationId
        , itemInventory   :: Maybe (EntityBase 'LocationT)
        } -> Entity 'ItemT

deriving instance Show (Entity a)
deriving instance Eq (Entity a)

-- World type stays mostly the same
data World = World
    { locations   :: Map LocationId (Entity 'LocationT)
    , actors      :: Map ActorId (Entity 'ActorT)
    , items       :: Map InventoryId (Entity 'ItemT)
    , activeActor :: Entity 'ActorT
    } deriving (Show, Eq)

-- New type class for accessing entity properties uniformly
class HasEntityBase (a :: EntityType) where
    getBase :: Entity a -> EntityBase a

instance HasEntityBase 'LocationT where
    getBase (Location base _) = base

instance HasEntityBase 'ActorT where
    getBase (Actor base _ _) = base

instance HasEntityBase 'ItemT where
    getBase (Item base _ _) = base

getId :: HasEntityBase a => Entity a -> EntityId
getId = entityId . getBase

getTags :: HasEntityBase a => Entity a -> Maybe [Text]
getTags = entityTags . getBase

getName :: HasEntityBase a => Entity a -> Text
getName = entityName . getBase

class Movable (a :: EntityType) where
    getLocationId :: Entity a -> EntityId
    setLocationId :: LocationId -> Entity a -> Entity a

instance Movable 'ActorT where
    getLocationId (Actor _ loc _) = loc
    setLocationId newLoc (Actor base _ inv) = Actor base newLoc inv

instance Movable 'ItemT where
    getLocationId (Item _ loc _) = loc
    setLocationId newLoc (Item base _ inv) = Item base newLoc inv

data MovablesRecord = MovablesRecord
    { movableItems  :: [Entity 'ItemT]
    , movableActors :: [Entity 'ActorT]
    }

findLocationById :: LocationId -> World -> Maybe (Entity 'LocationT)
findLocationById targetId = Map.lookup targetId . locations

findActorById :: ActorId -> World -> Maybe (Entity 'ActorT)
findActorById targetId = Map.lookup targetId . actors

findItemById :: ItemId -> World -> Maybe (Entity 'ItemT)
findItemById targetId = Map.lookup targetId . items

isContainer :: Entity a -> Bool
isContainer (Location {})       = True
isContainer (Actor {})          = True
isContainer (Item _ _ (Just _)) = True
isContainer (Item _ _ Nothing)  = False

getEntityName :: Entity a -> Text
getEntityName entity = case entity of
    Location base _ -> entityName base
    Actor base _ _  -> entityName base
    Item base _ _   -> entityName base

getEntityId :: Entity a -> EntityId
getEntityId entity = case entity of
    Location base _ -> entityId base
    Actor base _ _  -> entityId base
    Item base _ _   -> entityId base

-- | EntityResult
{- Example usage:
case findEntityById targetId world of
    Nothing -> handleNotFound targetId
    Just (LocResult loc) -> handleLocation loc
    Just (ActorResult actor) -> handleActor actor
    Just (ItemResult item) -> handleItem item
-}
data EntityResult =
    LocResult (Entity 'LocationT) |
    ActorResult (Entity 'ActorT) |
    ItemResult (Entity 'ItemT)

findEntityById :: EntityId -> World -> Maybe EntityResult
findEntityById targetId world =
    case Map.lookup targetId (locations world) of
        Just loc -> Just (LocResult loc)
        Nothing -> case Map.lookup targetId (actors world) of
            Just actor -> Just (ActorResult actor)
            Nothing -> case Map.lookup targetId (items world) of
                Just item -> Just (ItemResult item)
                Nothing   -> Nothing

-- Helper functions if needed
isLocation :: Maybe EntityResult -> Bool
isLocation (Just (LocResult _)) = True
isLocation _                    = False

isActor :: Maybe EntityResult -> Bool
isActor (Just (ActorResult _)) = True
isActor _                      = False

isItem :: Maybe EntityResult -> Bool
isItem (Just (ItemResult _)) = True
isItem _                     = False

getLocation :: Maybe EntityResult -> Maybe (Entity 'LocationT)
getLocation (Just (LocResult loc)) = Just loc
getLocation _                      = Nothing

getActor :: Maybe EntityResult -> Maybe (Entity 'ActorT)
getActor (Just (ActorResult actor)) = Just actor
getActor _                          = Nothing

getItem :: Maybe EntityResult -> Maybe (Entity 'ItemT)
getItem (Just (ItemResult item)) = Just item
getItem _                        = Nothing
