{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Entity.EntityConverter (EntityConversionError (..), convertToEntityWorld) where

import           Core.State.JSONTypes (EntityJSON (..), WorldJSON (..))
import qualified Core.State.JSONTypes as JSON (Location(..))  -- Qualified import
import           Data.Map             as Map
import qualified Data.Set             as Set
import           Data.Text            (Text)
import           Entity.Entity
import Prelude as P


data EntityConversionError
    = DuplicateId Text
    | MissingStartingActor Text
    | InvalidLocationReference Text
    | InvalidContainerReference Text
    deriving (Show, Eq)

-- | Convert from a GameEnvironment to our new Entity-based World
convertToEntityWorld :: WorldJSON -> Either EntityConversionError World
convertToEntityWorld WorldJSON{..} = do
    -- First validate all IDs are unique
    validateUniqueIds jLocations jPlayableActors jItems

    -- Create all locations
    let locEntities = P.map convertLocation jLocations
        locMap = Map.fromList [(getId loc, loc) | loc <- locEntities]

    -- Convert actors, referencing the new location IDs
    actorEntities <- traverse (convertActorWithLoc locMap) jPlayableActors
    let actorMap = Map.fromList [(getId act, act) | act <- actorEntities]

    -- Convert items, handling container references
    itemEntities <- traverse (convertItemWithLoc locMap actorMap) jItems
    let itemMap = Map.fromList [(getId item, item) | item <- itemEntities]

    Right $ World locMap actorMap itemMap

validateUniqueIds :: [JSON.Location] -> [EntityJSON] -> [EntityJSON] -> Either EntityConversionError ()
validateUniqueIds locs actors items =
    let allIds = Set.fromList $
            P.map JSON.locTag locs ++     -- Remove L. qualifier
            P.map jTag actors ++
            P.map jTag items
        totalCount = length locs + length actors + length items
    in if Set.size allIds /= totalCount
       then Left $ DuplicateId "Found duplicate IDs in input data"
       else Right ()

convertLocation :: JSON.Location -> Entity 'LocationT
convertLocation loc =
    Location                             -- This Location is from Entity.Entity
        { locationBase = EntityBase
            { entityId = EntityId (JSON.locTag loc)    -- Use qualified access
            , entityTag = JSON.locTag loc              -- Use qualified access
            , entityName = JSON.locName loc            -- Use qualified access
            }
        , destinations = P.map EntityId (JSON.destinationTags loc)  -- Use qualified access
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
fromMaybe _ (Just x)  = x