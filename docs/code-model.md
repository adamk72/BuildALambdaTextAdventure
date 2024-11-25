
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

# Game Concepts


