{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module Core.JSON.EntityConverter (convertToEntityWorld) where

import           Core.JSON.ScenarioConverter (convertToScenario)
import           Core.JSON.Types             (EntityJSON (..), WorldJSON (..))
import qualified Core.JSON.Types             as JSON (Location (..))
import           Data.Map                    as Map
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import           Entity.Class.EntityBase     (getId)
import           Entity.Entity
import           Entity.Types.Capacity       (Capacity (..))
import           Entity.Types.Common
import           Prelude                     as P
import           Scenario.Types

defaultActorCapacity :: Int
defaultActorCapacity = 3

defaultItemCapacity :: Int
defaultItemCapacity = 5

type EntityConversionError = Text

convertToEntityWorld :: WorldJSON -> Either EntityConversionError World
convertToEntityWorld WorldJSON {..} = do
  validateUniqueIds jLocations jPlayableActors jItems

  let locEntities = P.map convertLocation jLocations
      locMap = Map.fromList [(getId loc, loc) | loc <- locEntities]

  actorEntities <- traverse (convertActorWithLoc locMap) jPlayableActors
  let actorMap = Map.fromList [(getId act, act) | act <- actorEntities]

  let containerItems = P.filter (fromMaybe False . jHasInventorySlot) jItems
      contentItems = P.filter (not . fromMaybe False . jHasInventorySlot) jItems

  containerEntities <- traverse (convertItemWithLoc locMap actorMap Map.empty) containerItems
  let containerMap = Map.fromList [(getId item, item) | item <- containerEntities]

  contentEntities <- traverse (convertItemWithLoc locMap actorMap containerMap) contentItems

  let itemMap = Map.fromList $ [(getId item, item) | item <- containerEntities ++ contentEntities]

  scenarioResults <- case jScenarios of
    Just scenarios -> traverse convertToScenario scenarios
    Nothing        -> Right []
  let scenarioMap = Map.fromList [(Scenario.Types.tag scenario, scenario) | scenario <- scenarioResults]

  let activeActorId = EntityId jStartingActorTag
  activeActor <- case Map.lookup activeActorId actorMap of
    Just actor -> Right actor
    Nothing    -> Left $ "Missing starting actor for this tag: " <> jStartingActorTag

  Right $ World locMap actorMap itemMap activeActor scenarioMap

validateUniqueIds :: [JSON.Location] -> [EntityJSON] -> [EntityJSON] -> Either EntityConversionError ()
validateUniqueIds locs actors items =
  let allIds =
        Set.fromList $
          P.map JSON.locTag locs
            ++ P.map jTag actors
            ++ P.map jTag items
      totalCount = length locs + length actors + length items
   in if Set.size allIds /= totalCount
        then Left "Found duplicate IDs in input data"
        else Right ()

convertLocation :: JSON.Location -> Entity 'LocationT
convertLocation loc =
  Location
    { locationBase =
        EntityBase
          { entityId = EntityId (JSON.locTag loc),
            entityTags = JSON.locTags loc,
            entityName = JSON.locName loc
          },
      destinations = P.map EntityId (JSON.destinationTags loc),
      locationCapacity = Unlimited
    }

convertActorWithLoc :: Map LocationId (Entity 'LocationT) -> EntityJSON -> Either EntityConversionError (Entity 'ActorT)
convertActorWithLoc locMap json =
  case jLocTag json of
    Nothing -> Left $ "Invalid location reference from character list: " <> jTag json
    Just locTag ->
      let locId = EntityId locTag
       in if locId `Map.member` locMap
            then
              Right $
                Actor
                  { actorBase =
                      EntityBase
                        { entityId = EntityId (jTag json),
                          entityTags = jTags json,
                          entityName = jName json
                        },
                    actorLocationId = locId,
                    actorCapacity = Limited defaultActorCapacity -- Todo: allow this to change
                  }
            else Left $ "Location tag: " <> locTag <> " is invalid reference for a character."

convertItemWithLoc ::
  Map LocationId (Entity 'LocationT) ->
  Map ActorId (Entity 'ActorT) ->
  Map ItemId (Entity 'ItemT) ->
  EntityJSON ->
  Either EntityConversionError (Entity 'ItemT)
convertItemWithLoc locMap actorMap itemMap json =
  case jLocTag json of
    Nothing -> Left $ "Invalid location reference from item list: " <> jTag json
    Just locTag ->
      let containerId = EntityId locTag
          containerExists =
            containerId `Map.member` locMap
              || containerId `Map.member` actorMap
              || containerId `Map.member` itemMap
       in if containerExists
            then
              Right $
                Item
                  { itemBase =
                      EntityBase
                        { entityId = EntityId (jTag json),
                          entityTags = jTags json,
                          entityName = jName json
                        },
                    itemLocationId = containerId,
                    itemCapacity =
                      if fromMaybe False (jHasInventorySlot json)
                        then Just (Limited defaultItemCapacity)
                        else Nothing
                  }
            else Left $ "Location tag: " <> locTag <> " is not a valid container in this world."

fromMaybe :: a -> Maybe a -> a
fromMaybe def Nothing = def
fromMaybe _ (Just x)  = x
