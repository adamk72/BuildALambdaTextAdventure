
# Concepts and Definitions

## Preliminary Concepts and Terms

### Clarifications

These are terms I'm purposely calling out because in terms of games and programming languages, respectively, they are over-burdened with meaning.

- The term "character" is used to describe any human or human-like being in the game. The initial setup of the game in the JSON file refers to characters, and the term is used in the dialogs of the the game. However, a "character" is not a formal game object, in code (other than being used as an arbitrary variable). Characters are represented by the Actor Entity.
- "Object" is a term used for conversation at the meta-programming, philosophical, and grammatical levels. It is not normally referring to a coding structure just as Java or JS object, unless context clearly indicates it to be such a structure.

## Game Objects

### Tags

Tags are human-readable strings. They play a role in making defining the adventure in the JSON file easy and human-friendly, and they are parsed in the REPL during the command interpretation phase.

Internally, the concept of the tag is short-lived, as every tag should be converted to Entity type. Doing this will make the coding more streamlined, instead of having to look up tags all the time to convert to an Entity anyway to get the object's more detailed information.

### Entities: Actors and Items

There are two types of Entities in the game world:

- An Actor, which represents both the playable character of the game player and also represents the non-player character elements of the game; specifically, those elements that can be meaningfully interacted through commands like 'talk' and 'attack'. Actors can change Locations through the 'go' command.
- An Item represents an in game object that the active character can interact with through commands like 'get' and 'open'. They can sometimes be picked up and carried in the Inventory of an Actor and do not move Locations unless carried as such. These usually are considered inanimate objects in the real world, but for the sake of the game, concepts like pets, wild animals, and homunculi may be included.

### Locations, Capacity, and Containers

A Location is the high level representation of space in the game world. Alone, without additional context, a `Location` type represents a physical space in the game world (like "meadow", "cave")

A Container is a Location, Actor, or Item that has Capacity. Capacity is whether or not an Entity can store/carry something. Locations can store infinite objects of both Actor and Item type. All Actors can stores Items, but to a limited amount. Some Items can also store a limited number of other Items.

## Game Concepts


# **** TEMP ****

A suggestion of how to re-write Operations.hs based on the new Entity.hs file from ChatGPT. It's broken in a few places, demonstrates a radically different way of writing the Haskell compared to the more conservative mode of Claude (which is part of a constraint I think I put on it for the project I setup).
```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Core.State.Operations (module Core.State.Operations) where

import Entity.Entity
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Maybe (mapMaybe)
import Data.List (find)

-- | Find an entity in a map by a given predicate
findInWorld :: (Entity a -> Bool) -> Map.Map EntityId (Entity a) -> Maybe (Entity a)
findInWorld predicate = find predicate . Map.elems

-- | Helper to lookup an entity and wrap it in the appropriate Either constructors
lookupEntity :: EntityId -> Map.Map EntityId (Entity a) -> (Entity a -> b) -> (b -> c) -> Maybe c
lookupEntity eid map wrap wrapOuter = fmap (wrapOuter . wrap) (Map.lookup eid map)

-- CHATGPT
findEntityById :: EntityId -> World -> Maybe (Either (Either (Entity 'LocationT) (Entity 'ActorT)) (Entity 'ItemT))
findEntityById eid world =
    lookupEntity eid (locations world) Left Left
    <|> lookupEntity eid (actors world) (Left . Right) Right
    <|> lookupEntity eid (items world) Right Right

-- CLAUDE
findEntityById :: EntityId -> World -> Maybe (Either (Either (Entity 'LocationT) (Entity 'ActorT)) (Entity 'ItemT))
findEntityById eid world =
    case Map.lookup eid (locations world) of
        Just loc -> Just $ Left $ Left loc
        Nothing -> case Map.lookup eid (actors world) of
            Just act -> Just $ Left $ Right act
            Nothing -> case Map.lookup eid (items world) of
                Just item -> Just $ Right item
                Nothing   -> Nothing
-- END CLAUDE

-- | Find an entity by its tag across world maps
findEntityByTag :: Text -> World -> Maybe (Either (Either (Entity 'LocationT) (Entity 'ActorT)) (Entity 'ItemT))
findEntityByTag tag world =
    findEntityBy (getTag >>> (== tag)) world

-- CLAUDE
findEntityByTag :: Text -> World -> Maybe (Either (Either (Entity 'LocationT) (Entity 'ActorT)) (Entity 'ItemT))
findEntityByTag tag world =
    let findInMap = find (\e -> getTag e == tag)
    in case findInMap (locations world) of
        Just loc -> Just $ Left $ Left loc
        Nothing -> case findInMap (actors world) of
            Just act -> Just $ Left $ Right act
            Nothing -> case findInMap (items world) of
                Just item -> Just $ Right item
                Nothing   -> Nothing
-- END CLAUDE

-- | Helper to find an entity by a general predicate
findEntityBy :: (Entity a -> Bool) -> World -> Maybe (Either (Either (Entity 'LocationT) (Entity 'ActorT)) (Entity 'ItemT))
findEntityBy predicate world =
    fmap (Left . Left) (findInWorld predicate (locations world)) <|>
    fmap (Left . Right) (findInWorld predicate (actors world)) <|>
    fmap Right (findInWorld predicate (items world))

-- | Update the location of a movable entity
updateLocation :: (Movable a) => EntityId -> Entity a -> World -> World
updateLocation newLoc entity world =
    updateEntityMap (getId entity) (updateLocationField newLoc) (entityMap world entity)

-- | Update the location field of an entity based on type
updateLocationField :: EntityId -> Entity a -> Entity a
updateLocationField newLoc entity = case entity of
    Actor{} -> entity { actorLocation = newLoc }
    Item{}  -> entity { itemLocation = newLoc }

-- | General function to update an entity in the relevant map
updateEntityMap :: EntityId -> (Entity a -> Entity a) -> Map.Map EntityId (Entity a) -> Map.Map EntityId (Entity a)
updateEntityMap eid updateFn = Map.adjust updateFn eid

-- | Get all entities at a specific location
getEntitiesAtLoc :: EntityId -> World -> [Either (Entity 'ActorT) (Entity 'ItemT)]
getEntitiesAtLoc locId world =
    let actorsAtLoc = map Left $ Map.elems $ Map.filter ((== locId) . getLocation) (actors world)
        itemsAtLoc  = map Right $ Map.elems $ Map.filter ((== locId) . getLocation) (items world)
    in actorsAtLoc ++ itemsAtLoc

-- Other functions like getItemsAtLoc, addToContainer, removeFromContainer, etc., would follow similar refactoring patterns.
```


