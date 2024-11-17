{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
module Entity.Entity (module Entity.Entity) where

import           Data.Map  as Map
import           Data.Text

-- Core types
data EntityType = LocationT | ActorT | ItemT

newtype EntityId = EntityId { unEntityId :: Text }
    deriving (Show, Eq, Ord)

data EntityBase (a :: EntityType) = EntityBase
    { entityId   :: EntityId
    , entityTags :: Maybe [Text]
    , entityName :: Text
    } deriving (Show, Eq)

-- The main Entity GADT
data Entity (a :: EntityType) where
    Location ::
        { locationBase   :: EntityBase 'LocationT
        , destinations   :: [EntityId]
        } -> Entity 'LocationT

    Actor ::
        { actorBase      :: EntityBase 'ActorT
        , actorLocation  :: EntityId
        , actorInventory :: EntityBase 'LocationT
        } -> Entity 'ActorT

    Item ::
        { itemBase      :: EntityBase 'ItemT
        , itemLocation  :: EntityId
        , itemInventory :: Maybe (EntityBase 'LocationT)
        } -> Entity 'ItemT

deriving instance Show (Entity 'LocationT)
deriving instance Show (Entity 'ActorT)
deriving instance Show (Entity 'ItemT)

deriving instance Eq (Entity 'LocationT)
deriving instance Eq (Entity 'ActorT)
deriving instance Eq (Entity 'ItemT)

-- World type that stores all entities
data World = World
    { locations   :: Map EntityId (Entity 'LocationT)
    , actors      :: Map EntityId (Entity 'ActorT)
    , items       :: Map EntityId (Entity 'ItemT)
    , activeActor :: Entity 'ActorT
    } deriving (Show, Eq)

-- Instead of trying to return a polymorphic Entity a, we'll use a GADT to wrap the different types
data SomeEntity where
    SomeEntity :: Entity a -> SomeEntity

-- Type classes for entity behaviors
class Tagged (a :: EntityType) where
    getId   :: Entity a -> EntityId
    getTags :: Entity a -> Maybe [Text]
    getName :: Entity a -> Text

class Movable (a :: EntityType) where
    getLocationId :: Entity a -> EntityId
    setLocationId :: EntityId -> Entity a -> Entity a

instance Tagged 'LocationT where
    getId (Location base _) = entityId base
    getTags (Location base _) = entityTags base
    getName (Location base _) = entityName base

instance Tagged 'ActorT where
    getId (Actor base _ _) = entityId base
    getTags (Actor base _ _) = entityTags base
    getName (Actor base _ _) = entityName base

instance Tagged 'ItemT where
    getId (Item base _ _) = entityId base
    getTags (Item base _ _) = entityTags base
    getName (Item base _ _) = entityName base

instance Movable 'ActorT where
    getLocationId (Actor _ loc _) = loc
    setLocationId newLoc (Actor base _ inv) = Actor base newLoc inv

instance Movable 'ItemT where
    getLocationId (Item _ loc _) = loc
    setLocationId newLoc (Item base _ inv) = Item base newLoc inv

getAllEntitiesOfType :: World -> (World -> Map EntityId (Entity a)) -> [Entity a]
getAllEntitiesOfType world getter = elems (getter world)

getAllLocations :: World -> [Entity 'LocationT]
getAllLocations world = getAllEntitiesOfType world locations

getAllActors :: World -> [Entity 'ActorT]
getAllActors world = getAllEntitiesOfType world actors

getAllItems :: World -> [Entity 'ItemT]
getAllItems world = getAllEntitiesOfType world items

isContainer ::  Entity a -> Bool
isContainer (Location {})       = True
isContainer (Actor {})          = True
isContainer (Item _ _ (Just _)) = True
isContainer (Item _ _ Nothing)  = False

getEntityName :: Entity a -> Text
getEntityName entity = case entity of
    Location base _ -> entityName base
    Actor base _ _  -> entityName base
    Item base _ _   -> entityName base
