{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Use join" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
module Entity.Ops.Location (module Entity.Ops.Location) where
import           Data.List               as List (find)
import qualified Data.Map                as Map
import           Data.Maybe
import           Data.Text
import           Entity.Class.EntityBase
import           Entity.Class.Viewable   (ViewablesRecord (..), getLocationId, getViewablesRecordByLocId)
import           Entity.Entity
import           Entity.Types.Common
import           Prelude                 as P
import           Utils

getLocationDestinations :: LocationId -> World -> Maybe [LocationId]
getLocationDestinations locId w = destinations <$> Map.lookup locId (locations w)

findItemIdAtActorLoc :: ItemId -> World -> Maybe ItemId
findItemIdAtActorLoc iId w =
    let acLocId = getLocationId (activeActor w)
        itemIds = P.map getId $ Map.elems $ Map.filter (\loc -> getLocationId loc == acLocId) (items w)
    in List.find (== iId) itemIds

findEntityIdAtActorLoc :: EntityId -> World -> Maybe EntityId
findEntityIdAtActorLoc eId w =
    let movableIds = getViewableIdsAtActorLoc w
        acLocId = getLocationId (activeActor w)
        locationIds = P.map getId $ Map.elems $ Map.filter (\loc -> getId loc == acLocId) (locations w)
        allVisibleIds = movableIds ++ locationIds
    in List.find (== eId) allVisibleIds

getViewableIdsAtActorLoc :: World -> [MovableId]
getViewableIdsAtActorLoc w =
    let acLocId = getLocationId (activeActor w)
        ViewablesRecord {viewableItems = items, viewableActors = actors} = getViewablesRecordByLocId acLocId w
    in P.map getId items ++ P.map getId actors

getViewableNamesAtActorLoc :: World -> [Text]
getViewableNamesAtActorLoc w =
    let acLocId = getLocationId (activeActor w)
        ViewablesRecord {viewableItems = items, viewableActors = actors} = getViewablesRecordByLocId acLocId w
    in P.map getName items ++ P.map getName actors

getActiveActorLocation :: World -> Entity 'LocationT
getActiveActorLocation w =
    let actorLocId = getLocationId (activeActor w)
    in fromJust (findLocationById actorLocId w)

getActiveActorLocationName :: World -> Text
getActiveActorLocationName w = getName (getActiveActorLocation w)

getActiveActorLocationId :: World -> LocationId
getActiveActorLocationId = getLocationId . activeActor

getActorsAtLocation :: LocationId -> World -> [Entity 'ActorT]
getActorsAtLocation locId world = viewableActors $ getViewablesRecordByLocId locId world

-- If you need to format actor names at a location
getActorNamesAtLocation :: LocationId -> World -> [Text]
getActorNamesAtLocation locId world =
    P.map getName $ viewableActors $ getViewablesRecordByLocId locId world

-- If you need both items and actors but want to process them separately
processLocationContents :: LocationId -> World -> Text
processLocationContents locId world =
    let ViewablesRecord {viewableItems = items, viewableActors = actors} =
            getViewablesRecordByLocId locId world
    in "You see " <> formatActors actors <> " and " <> formatItems items
  where
    formatActors []     = "no one here"
    formatActors actors = oxfordComma (P.map getName actors)

    formatItems []    = "nothing else"
    formatItems items = oxfordComma (P.map getName items)
