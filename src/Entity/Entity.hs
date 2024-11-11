{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
module Entity.Entity (module Entity.Entity) where
import           Data.Map
import           Data.Text

data EntityType = LocationT | ActorT | ItemT

newtype EntityId = EntityId { unEntityId :: Text }
    deriving (Show, Eq, Ord)

data EntityBase (a :: EntityType) = EntityBase
    { entityId   :: EntityId
    , entityTag  :: Text
    , entityName :: Text
    } deriving (Show, Eq)

data Entity (a :: EntityType) where
    Location ::
        { locationBase     :: EntityBase 'LocationT
        , destinations :: [EntityId]
        } -> Entity 'LocationT

    Actor ::
        { actorBase      :: EntityBase 'ActorT
        , actorLocation  :: EntityId
        , actorContents :: [EntityId] -- "inventory"
        } -> Entity 'ActorT

    Item ::
        { itemBase       :: EntityBase 'ItemT
        , itemLocation   :: EntityId
        , itemContents :: Maybe [EntityId]
        } -> Entity 'ItemT

class Tagged (a :: EntityType) where
    getId   :: Entity a -> EntityId
    getTag  :: Entity a -> Text
    getName :: Entity a -> Text

class Movable (a :: EntityType) where
    getLocation :: Entity a -> EntityId

class Container (a :: EntityType) where
    getContents :: Entity a -> [EntityId]

instance Tagged 'LocationT where
    getId (Location base _) = entityId base
    getTag (Location base _) = entityTag base
    getName (Location base _) = entityName base

instance Tagged 'ActorT where
    getId (Actor base _ _) = entityId base
    getTag (Actor base _ _) = entityTag base
    getName (Actor base _ _) = entityName base

instance Tagged 'ItemT where
    getId (Item base _ _) = entityId base
    getTag (Item base _ _) = entityTag base
    getName (Item base _ _) = entityName base

instance Movable 'ActorT where
    getLocation (Actor _ loc _) = loc

instance Movable 'ItemT where
    getLocation (Item _ loc _) = loc

instance Container 'LocationT where
    getContents (Location _ contents) = contents

instance Container 'ActorT where
    getContents (Actor _ _ inv) = inv

instance Container 'ItemT where
    getContents (Item _ _ (Just contents)) = contents
    getContents (Item _ _ Nothing)         = []

data World = World
    { locations :: Map EntityId (Entity 'LocationT)
    , actors    :: Map EntityId (Entity 'ActorT)
    , items     :: Map EntityId (Entity 'ItemT)
    }

isContainer :: Entity a -> Bool
isContainer (Location _ _)      = True
isContainer (Actor {})          = True
isContainer (Item _ _ (Just _)) = True
isContainer (Item _ _ Nothing)  = False
