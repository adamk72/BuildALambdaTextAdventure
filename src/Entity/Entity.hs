{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures     #-}
module Entity.Entity (module Entity.Entity) where

import           Data.Map as Map
import           Data.Text
import Prelude as P

-- Core types
data EntityType = LocationT | ActorT | ItemT

newtype EntityId = EntityId { unEntityId :: Text }
    deriving (Show, Eq, Ord)

data EntityBase (a :: EntityType) = EntityBase
    { entityId   :: EntityId
    , entityTags :: [Text]
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
        , actorInventory :: [EntityId]
        } -> Entity 'ActorT

    Item ::
        { itemBase      :: EntityBase 'ItemT
        , itemLocation  :: EntityId
        , itemInventory :: Maybe [EntityId]
        } -> Entity 'ItemT

deriving instance Show (Entity 'LocationT)
deriving instance Show (Entity 'ActorT)
deriving instance Show (Entity 'ItemT)

deriving instance Eq (Entity 'LocationT)
deriving instance Eq (Entity 'ActorT)
deriving instance Eq (Entity 'ItemT)

-- Type classes for entity behaviors
class Tagged (a :: EntityType) where
    getId   :: Entity a -> EntityId
    getTags :: Entity a -> [Text]
    getName :: Entity a -> Text

class Movable (a :: EntityType) where
    getLocation :: Entity a -> EntityId
    setLocation :: EntityId -> Entity a -> Entity a

class Container (a :: EntityType) where
    getContents :: Entity a -> [EntityId]

-- Type class instances remain the same
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
    getLocation (Actor _ loc _) = loc
    setLocation newLoc (Actor base _ inv) = Actor base newLoc inv

instance Movable 'ItemT where
    getLocation (Item _ loc _) = loc
    setLocation newLoc (Item base _ inv) = Item base newLoc inv

instance Container 'LocationT where
    getContents (Location _ contents) = contents

instance Container 'ActorT where
    getContents (Actor _ _ inv) = inv

instance Container 'ItemT where
    getContents (Item _ _ (Just contents)) = contents
    getContents (Item _ _ Nothing)         = []


-- World type that stores all entities
data World = World
    { locations   :: Map EntityId (Entity 'LocationT)
    , actors      :: Map EntityId (Entity 'ActorT)
    , items       :: Map EntityId (Entity 'ItemT)
    , activeActor :: Entity 'ActorT
    }

-- Instead of trying to return a polymorphic Entity a, we'll use a GADT to wrap the different types
data SomeEntity where
    SomeEntity :: Entity a -> SomeEntity

getAllEntitiesOfType :: World -> (World -> Map EntityId (Entity a)) -> [Entity a]
getAllEntitiesOfType world getter = elems (getter world)

getAllLocations :: World -> [Entity 'LocationT]
getAllLocations world = getAllEntitiesOfType world locations

getAllActors :: World -> [Entity 'ActorT]
getAllActors world = getAllEntitiesOfType world actors

getAllItems :: World -> [Entity 'ItemT]
getAllItems world = getAllEntitiesOfType world items

isContainer ::  Entity a -> Bool
isContainer (Location {}) = True
isContainer (Actor {}) = True
isContainer (Item _ _ (Just _)) = True
isContainer (Item _ _ Nothing) = False

getEntityName :: Entity a -> Text
getEntityName entity = case entity of
    Location base _ -> entityName base
    Actor base _ _ -> entityName base
    Item base _ _ -> entityName base