{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}

module Entity.Entity
    ( Entity (..)
    , EntityBase (..)
    , EntityResult (..)
    , EntityType (..)
    , World (..)
    , findEntityById
    , findItemById
    , findLocationById
    , isContainer
    ) where

import           Data.Map              as Map
import           Data.Text             (Text)
import           Entity.Types.Capacity (Capacity (..))
import           Entity.Types.Common
import           Scenario.Types

data EntityType = LocationT | ActorT | ItemT

data EntityBase (a :: EntityType) = EntityBase
  { entityId   :: EntityId,
    entityTags :: Maybe [Text],
    entityName :: Text
  }
  deriving (Show, Eq)

data Entity (a :: EntityType) where
  Location ::
    { locationBase :: EntityBase 'LocationT,
      destinations :: [LocationId],
      locationCapacity :: Capacity
    } ->
    Entity 'LocationT
  Actor ::
    { actorBase :: EntityBase 'ActorT,
      actorLocationId :: LocationId,
      actorCapacity :: Capacity
    } ->
    Entity 'ActorT
  Item ::
    { itemBase :: EntityBase 'ItemT,
      itemLocationId :: LocationId,
      itemCapacity :: Maybe Capacity
    } ->
    Entity 'ItemT

deriving instance Show (Entity a)

deriving instance Eq (Entity a)

data World = World
  { locations   :: Map LocationId (Entity 'LocationT),
    actors      :: Map ActorId (Entity 'ActorT),
    items       :: Map InventoryId (Entity 'ItemT),
    activeActor :: Entity 'ActorT,
    scenarios   :: Map EntityId Scenario
  }
  deriving (Show, Eq)

findLocationById :: LocationId -> World -> Maybe (Entity 'LocationT)
findLocationById targetId = Map.lookup targetId . locations

findItemById :: ItemId -> World -> Maybe (Entity 'ItemT)
findItemById targetId = Map.lookup targetId . items

isContainer :: Entity a -> Bool
isContainer (Location {})       = True
isContainer (Actor {})          = True
isContainer (Item _ _ (Just _)) = True
isContainer (Item _ _ Nothing)  = False

-- | EntityResult

{- Example usage:
case findEntityById targetId world of
    Nothing -> handleNotFound targetId
    Just (LocationResult loc) -> handleLocation loc
    Just (ActorResult actor) -> handleActor actor
    Just (ItemResult item) -> handleItem item
-}
data EntityResult
  = LocationResult (Entity 'LocationT)
  | ActorResult (Entity 'ActorT)
  | ItemResult (Entity 'ItemT)

findEntityById :: EntityId -> World -> Maybe EntityResult
findEntityById targetId world =
  case Map.lookup targetId (locations world) of
    Just loc -> Just (LocationResult loc)
    Nothing -> case Map.lookup targetId (actors world) of
      Just actor -> Just (ActorResult actor)
      Nothing -> case Map.lookup targetId (items world) of
        Just item -> Just (ItemResult item)
        Nothing   -> Nothing
