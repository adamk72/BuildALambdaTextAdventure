{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
module Entity.Entity (module Entity.Entity) where

import           Data.Map              as Map
import           Data.Text             (Text)
import           Entity.Types.Capacity (Capacity (..))
import           Entity.Types.Common
import           Scenario.Types

data EntityType = LocationT | ActorT | ItemT

data EntityBase (a :: EntityType) = EntityBase
    { entityId   :: EntityId
    , entityTags :: Maybe [Text]
    , entityName :: Text
    } deriving (Show, Eq)

data Entity (a :: EntityType) where
    Location ::
        { locationBase     :: EntityBase 'LocationT
        , destinations     :: [LocationId]
        , locationCapacity :: Capacity
        } -> Entity 'LocationT

    Actor ::
        { actorBase        :: EntityBase 'ActorT
        , actorLocationId  :: LocationId
        , actorCapacity    :: Capacity
        } -> Entity 'ActorT

    Item ::
        { itemBase        :: EntityBase 'ItemT
        , itemLocationId  :: LocationId
        , itemCapacity    :: Maybe Capacity
        } -> Entity 'ItemT

deriving instance Show (Entity a)
deriving instance Eq (Entity a)

data World = World
    { locations   :: Map LocationId (Entity 'LocationT)
    , actors      :: Map ActorId (Entity 'ActorT)
    , items       :: Map InventoryId (Entity 'ItemT)
    , activeActor :: Entity 'ActorT
    , scenarios   :: Map EntityId Scenario
    } deriving (Show, Eq)

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

-- | EntityResult
{- Example usage:
case findEntityById targetId world of
    Nothing -> handleNotFound targetId
    Just (LocationResult loc) -> handleLocation loc
    Just (ActorResult actor) -> handleActor actor
    Just (ItemResult item) -> handleItem item
-}
data EntityResult =
    LocationResult (Entity 'LocationT) |
    ActorResult (Entity 'ActorT) |
    ItemResult (Entity 'ItemT)

findEntityById :: EntityId -> World -> Maybe EntityResult
findEntityById targetId world =
    case Map.lookup targetId (locations world) of
        Just loc -> Just (LocationResult loc)
        Nothing -> case Map.lookup targetId (actors world) of
            Just actor -> Just (ActorResult actor)
            Nothing -> case Map.lookup targetId (items world) of
                Just item -> Just (ItemResult item)
                Nothing   -> Nothing

isLocation :: Maybe EntityResult -> Bool
isLocation (Just (LocationResult _)) = True
isLocation _                         = False

isActor :: Maybe EntityResult -> Bool
isActor (Just (ActorResult _)) = True
isActor _                      = False

isItem :: Maybe EntityResult -> Bool
isItem (Just (ItemResult _)) = True
isItem _                     = False

getLocation :: Maybe EntityResult -> Maybe (Entity 'LocationT)
getLocation (Just (LocationResult loc)) = Just loc
getLocation _                           = Nothing

getActor :: Maybe EntityResult -> Maybe (Entity 'ActorT)
getActor (Just (ActorResult actor)) = Just actor
getActor _                          = Nothing

getItem :: Maybe EntityResult -> Maybe (Entity 'ItemT)
getItem (Just (ItemResult item)) = Just item
getItem _                        = Nothing
