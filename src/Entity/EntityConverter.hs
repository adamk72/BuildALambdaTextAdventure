{-# LANGUAGE OverloadedStrings #-}
module Entity.EntityConverter
    ( convertToEntityWorld
    , EntityConversionError(..)
    ) where

import           Entity.Entity
import           Core.State.GameState    (GameEnvironment(..), Metadata(..))
import           Core.State.JSON         (GameWorldJSON(..), EntityJSON(..))
import           Core.State.Location     (Location(..))
import qualified Data.Map               as Map
import qualified Data.Set               as Set
import           Data.Text              (Text)

data EntityConversionError
    = DuplicateId Text
    | MissingStartingActor Text
    | InvalidLocationReference Text
    | InvalidContainerReference Text
    deriving (Show, Eq)

-- | Convert from a GameEnvironment to our new Entity-based World
convertToEntityWorld :: GameEnvironment -> Either EntityConversionError World
convertToEntityWorld GameEnvironment{..} =
    case world of
        Nothing -> Left $ MissingStartingActor "No world data found"
        Just worldData -> convertGameWorldJSON worldData

-- | Convert from GameWorldJSON to World
convertGameWorldJSON :: GameWorldJSON -> Either EntityConversionError World
convertGameWorldJSON GameWorldJSON{..} = do
    -- First validate all IDs are unique
    validateUniqueIds jLocations jPlayableActors jItems

    -- Create all locations
    let locEntities = map convertLocation jLocations
        locMap = Map.fromList [(getId loc, loc) | loc <- locEntities]

    -- Convert actors, referencing the new location IDs
    actorEntities <- traverse (convertActorWithLoc locMap) jPlayableActors
    let actorMap = Map.fromList [(getId act, act) | act <- actorEntities]

    -- Convert items, handling container references
    itemEntities <- traverse (convertItemWithLoc locMap actorMap) jItems
    let itemMap = Map.fromList [(getId item, item) | item <- itemEntities]

    Right $ World locMap actorMap itemMap

validateUniqueIds :: [Location] -> [EntityJSON] -> [EntityJSON] -> Either EntityConversionError ()
validateUniqueIds locs actors items =
    let allIds = Set.fromList $
            map locTag locs ++
            map jTag actors ++
            map jTag items
        totalCount = length locs + length actors + length items
    in if Set.size allIds /= totalCount
       then Left $ DuplicateId "Found duplicate IDs in input data"
       else Right ()

convertLocation :: Location -> Entity 'LocationT
convertLocation loc =
    Location
        { locationBase = EntityBase
            { entityId = EntityId (locTag loc)
            , entityTag = locTag loc  -- For now, keeping same as ID
            , entityName = locName loc
            }
        , destinations = map EntityId (destinationTags loc)
        }

convertActorWithLoc :: Map EntityId (Entity 'LocationT)
                   -> EntityJSON
                   -> Either EntityConversionError (Entity 'ActorT)
convertActorWithLoc locMap json =
    case jLocTag json of
        Nothing -> Left $ InvalidLocationReference (jTag json)
        Just locTag ->
            let locId = EntityId locTag
            in if locId `Map.member` locMap
               then Right $ Actor
                    { actorBase = EntityBase
                        { entityId = EntityId (jTag json)
                        , entityTag = jTag json
                        , entityName = jName json
                        }
                    , actorLocation = locId
                    , actorContents = []
                    }
               else Left $ InvalidLocationReference locTag

convertItemWithLoc :: Map EntityId (Entity 'LocationT)
                  -> Map EntityId (Entity 'ActorT)
                  -> EntityJSON
                  -> Either EntityConversionError (Entity 'ItemT)
convertItemWithLoc locMap actorMap json =
    case jLocTag json of
        Nothing -> Left $ InvalidLocationReference (jTag json)
        Just locTag ->
            let containerId = EntityId locTag
            in if containerId `Map.member` locMap || containerId `Map.member` actorMap
               then Right $ Item
                    { itemBase = EntityBase
                        { entityId = EntityId (jTag json)
                        , entityTag = jTag json
                        , entityName = jName json
                        }
                    , itemLocation = containerId
                    , itemContents = if fromMaybe False (jHasInventorySlot json)
                                   then Just []
                                   else Nothing
                    }
               else Left $ InvalidLocationReference locTag

fromMaybe :: a -> Maybe a -> a
fromMaybe def Nothing = def
fromMaybe _ (Just x) = x