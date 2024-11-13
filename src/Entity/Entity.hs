{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE StandaloneDeriving #-}
module Entity.Entity (module Entity.Entity) where

import           Data.Map
import           Data.Text

data EntityType = LocationT | ActorT | ItemT

newtype EntityId = EntityId { unEntityId :: Text }
    deriving (Show, Eq, Ord)

data EntityBase (a :: EntityType) = EntityBase
    { entityId   :: EntityId
    , entityTags  :: [Text]
    , entityName :: Text
    } deriving (Show, Eq)

data Entity (a :: EntityType) where
    Location ::
        { locationBase   :: EntityBase 'LocationT
        , destinations   :: [EntityId]
        } -> Entity 'LocationT

    Actor ::
        { actorBase      :: EntityBase 'ActorT
        , actorLocation  :: EntityId
        , actorInventory  :: [EntityId]
        } -> Entity 'ActorT

    Item ::
        { itemBase      :: EntityBase 'ItemT
        , itemLocation  :: EntityId
        , itemInventory  :: Maybe [EntityId]
        } -> Entity 'ItemT

{-
Pattern Matching with Destructuring:
    We pattern match the entire constructor pattern, including the nested EntityBase pattern.
    We destructure the EntityBase pattern to extract the entityName field.
Accessing entityName:
    We directly access the entityName field from the destructured pattern.
-}
getEntityName :: Entity a -> Text
getEntityName entity =
  case entity of
    Location { locationBase = EntityBase { entityName } } -> entityName
    Actor { actorBase = EntityBase { entityName } }       -> entityName
    Item { itemBase = EntityBase { entityName } }         -> entityName

data AnyEntity where
    AnyLocation :: Entity 'LocationT -> AnyEntity
    AnyActor    :: Entity 'ActorT -> AnyEntity
    AnyItem     :: Entity 'ItemT -> AnyEntity

data AnyMovableEntity where
    AnyMovableActor    :: Entity 'ActorT -> AnyMovableEntity
    AnyMovableItem     :: Entity 'ItemT -> AnyMovableEntity

deriving instance Show AnyEntity
deriving instance Eq AnyEntity
deriving instance Show AnyMovableEntity
deriving instance Eq AnyMovableEntity

deriving instance Show (Entity 'LocationT)
deriving instance Show (Entity 'ActorT)
deriving instance Show (Entity 'ItemT)

deriving instance Eq (Entity 'LocationT)
deriving instance Eq (Entity 'ActorT)
deriving instance Eq (Entity 'ItemT)

class Tagged (a :: EntityType) where
    getId   :: Entity a -> EntityId
    getTags  :: Entity a -> [Text]
    getName :: Entity a -> Text

class Movable (a :: EntityType) where
    getLocation :: Entity a -> EntityId
    setLocation :: EntityId -> Entity a -> Entity a

class Container (a :: EntityType) where
    getContents :: Entity a -> [EntityId]

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
    setLocation newLoc (Actor base _ otherFields) = Actor base newLoc otherFields


instance Movable 'ItemT where
    getLocation (Item _ loc _) = loc
    setLocation newLoc (Item base _ otherFields) = Item base newLoc otherFields


instance Container 'LocationT where
    getContents (Location _ contents) = contents

instance Container 'ActorT where
    getContents (Actor _ _ inv) = inv

instance Container 'ItemT where
    getContents (Item _ _ (Just contents)) = contents
    getContents (Item _ _ Nothing)         = []

data World = World
    { locations     :: Map EntityId (Entity 'LocationT)
    , actors        :: Map EntityId (Entity 'ActorT)
    , items         :: Map EntityId (Entity 'ItemT)
    , activeActor :: Entity 'ActorT
    }


-- | Get all entities of a specific type from the world
getAllEntitiesOfType :: World -> (World -> Map EntityId (Entity a)) -> [Entity a]
getAllEntitiesOfType world getter = elems (getter world)

-- | Get all locations in the world
getAllLocations :: World -> [Entity 'LocationT]
getAllLocations world = getAllEntitiesOfType world locations

-- | Get all actors in the world
getAllActors :: World -> [Entity 'ActorT]
getAllActors world = getAllEntitiesOfType world actors

-- | Get all items in the world
getAllItems :: World -> [Entity 'ItemT]
getAllItems world = getAllEntitiesOfType world items

isContainer :: Entity a -> Bool
isContainer (Location _ _)      = True
isContainer (Actor {})          = True
isContainer (Item _ _ (Just _)) = True
isContainer (Item _ _ Nothing)  = False
