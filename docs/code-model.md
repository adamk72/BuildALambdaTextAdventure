
# Definitions

## Basic Terminology

### Clarifications

These are terms I'm purposely calling out because in terms of games and programming languages, respectively, they are over-burdened with meaning.

- The term "character" is used to describe any human or human-like being in the game. The initial setup of the game in the JSON file refers to characters, and the term is used in the dialogs of the the game. However, a "character" is not a formal game object, in code (other than being used as an arbitrary variable). Characters are represented by the `Actor` Entity.
- "Object" is a term used for conversation at the meta-programming, philosophical, and grammatical levels. It is not normally referring to a coding structure just as Java or JS object, unless context clearly indicates it to be such a structure.

## Game Objects

### Tags

Tags are human-readable strings. They play a role in making defining the adventure in the JSON file easy and human-friendly, and they are parsed in the REPL during the command interpretation phase.

Internally, the concept of the tag is short-lived, as every tag should be converted to Entity type. Doing this will make the coding more streamlined, instead of having to look up tags all the time to convert to an Entity anyway to get the object's more detailed information.

### Entities: Actors and Items

There are two types of Entities in the game world:

- An `Actor`, which represents both the playable character of the game player and also represents the non-player character elements of the game; specifically, those elements that can be meaningfully interacted through commands like 'talk' and 'attack'. Actors can change Locations through the 'go' command.
- An `Item` represents an in game object that the active character can interact with through commands like 'get' and 'open'. They can sometimes be picked up and carried in the Inventory of an `Actor` and do not move Locations unless carried as such. These usually are considered inanimate objects in the real world, but for the sake of the game, concepts like pets, wild animals, and homunculi may be included.

### Locations, Capacity, and Containers

A `Location` is the high level representation of space in the game world. Alone, without additional context, a `Location` type represents a physical space in the game world (like "meadow", "cave")

A `Container` is a `Location`, `Actor`, or `Item` that has `Capacity`. `Capacity` is whether or not an Entity can store/carry something. Locations can store an unlimited number of objects of both `Actor` and `Item` type. All Actors can stores Items, but to a limited amount. Some Items can also store a limited number of other Items.

# Game Logic

## Visibility

At the moment the concept of "visibility" is simply whether or not an item or other actor is in the same location at the active actor, or if an item is in the active actor's inventory.

Claude.ai offers the following as inspiration:

```haskell
data Visibility =
    -- | Openly visible to anyone in the location
    Visible |
    -- | Hidden but can be found with appropriate actions (search, detect magic, etc)
    Hidden {
        hiddenType :: HiddenType,
        difficulty :: Int  -- How hard it is to find
    } |
    -- | Completely invisible until specific conditions are met
    Concealed {
        conditions :: [RevealCondition]
    }
    deriving (Show, Eq)

-- | Different ways something can be hidden
data HiddenType =
    UnderObject |      -- Hidden under another item
    BehindObject |     -- Hidden behind something
    CamouflagedAs Text -- Appears as something else
    deriving (Show, Eq)

-- | Conditions that must be met to reveal something
data RevealCondition =
    HasItem Text |          -- Must possess specific item
    HasSkill Text Int |     -- Must have skill at certain level
    TimeOfDay TimeRange |   -- Only visible during certain times
    UseAction Text         -- Must use specific action to reveal
    deriving (Show, Eq)

data TimeRange = Day | Night | Dawn | Dusk
    deriving (Show, Eq)

-- | Represents the lock state of a container
data LockState =
    Unlocked |
    Locked {
        lockType :: LockType,
        lockComplexity :: Int  -- How hard it is to unlock
    }
    deriving (Show, Eq)

data LockType =
    KeyLock Text |     -- Requires specific key
    Combination Int |  -- Requires numeric combination
    MagicSeal Text |   -- Requires specific spell/skill
    Puzzle Text       -- Requires solving a puzzle
    deriving (Show, Eq)

-- | Extended Entity with visibility properties
data VisibleEntity = VisibleEntity {
    baseEntity :: Entity,
    visibility :: Visibility,
    lockState :: Maybe LockState  -- Only relevant for containers
} deriving (Show, Eq)
```

## Actions

The heart of the game is the ability for the player to take actions on various objects in the game. Listed here are some of the actions that can be taken:

### Basic Item Actions:

- Get/take an item from a location
- Drop an item at current location
- Look at an item
- Look inside a container to see its contents
- Put an item into a container
- Move (via Put) an item from one container to another

### Current Implementation Details:

- Actors have an inventory capacity (defaulted to 3 items)
- Containers (items with inventory slots) have capacity (defaulted to 5 items)
- Locations have unlimited capacity
- Both containers and locations can hold items
- Items can be containers themselves (like the "bag of holding" in the test world)

### Missing/Potential Item Management Features:

- Take/get items from containers
- Take multiple items at once (get all)
- Drop multiple items at once (drop all)
- Move multiple items between containers
- Stack similar items
- Remove/take items from containers
- Transfer items between actors
- Trade items between actors
- Give items to actors
- Sort items in containers
- Search for items across all accessible containers
- List all containers and their contents
- Show container capacity and remaining space
- Examine item properties/attributes
- Combine/merge items
- Split stackable items
- Compare items
- Show item weight/volume (if implemented)
- Show detailed inventory stats


