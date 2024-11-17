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

data EntityBase (a :: EntityType) = EntityBase
    { entityId   :: EntityId
    , entityTags :: Maybe [Text]
    , entityName :: Text
    } deriving (Show, Eq)

-- Entity GADT remains but we'll add helper functions for common operations
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

deriving instance Show (Entity a)
deriving instance Eq (Entity a)

-- World type stays mostly the same
data World = World
    { locations   :: Map EntityId (Entity 'LocationT)
    , actors      :: Map EntityId (Entity 'ActorT)
    , items       :: Map EntityId (Entity 'ItemT)
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
    setLocationId :: EntityId -> Entity a -> Entity a

instance Movable 'ActorT where
    getLocationId (Actor _ loc _) = loc
    setLocationId newLoc (Actor base _ inv) = Actor base newLoc inv

instance Movable 'ItemT where
    getLocationId (Item _ loc _) = loc
    setLocationId newLoc (Item base _ inv) = Item base newLoc inv

findLocationById :: EntityId -> World -> Maybe (Entity 'LocationT)
findLocationById targetId = Map.lookup targetId . locations

findActorById :: EntityId -> World -> Maybe (Entity 'ActorT)
findActorById targetId = Map.lookup targetId . actors

findItemById :: EntityId -> World -> Maybe (Entity 'ItemT)
findItemById targetId = Map.lookup targetId . items

findEntityById :: EntityId -> World -> Either Text (Either (Entity 'LocationT) (Either (Entity 'ActorT) (Entity 'ItemT)))
findEntityById targetId world =
    case (findLocationById targetId world, findActorById targetId world, findItemById targetId world) of
        (Just loc, _, _)   -> Right (Left loc)
        (_, Just actor, _) -> Right (Right (Left actor))
        (_, _, Just item)  -> Right (Right (Right item))
        _                  -> Left $ "Entity not found: " <> unEntityId targetId

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