# Definitions

- `phrase`              => any arbitrary sentence.
- `expression` (`exp`)  => the result of of parsing a phrase which is well structured. See forms below.
- `command` (`cmd`)     => phrase that starts with a known good verb (e.g., "go", "look").
- `clause`              => the portion of a command after the verb. There are two types:
  - 'noun clause'
  - 'prepositional clause'

# Command patterns

## Verb Phrase Parse rules

These assume that the first word, the verb, passes a check that they are valid for parsing; otherwise reject as "I don't know how to do "X".

Process:
- if encounter single verb, (V) pass to matching function.
- if word is an article, (a), skip to next word.
- if next word is _not_ a (p), keep checking next words until peek indicates a (p), then store.
- if encounter a preposition, (p), peek at the next word.
  - if the compound of the original (p) + the peek word are in the list of acceptable prepositions, peek the next word.
  - if new compound is till valid, continue to peek the next word.
  - otherwise, if no more compounds are valid, reduce to single word preposition for passing to matching function.

Return the following:
- verb                              -- inventory
- verb target                       -- go cave; look north (essentially ignore the preposition)
- verb object target                -- put bauble bag (also ignore the preposition)
- verb preposition target           -- hide [self] under table; look _at_ bag; look _in_ bag
- verb object preposition target    -- put bauble in bag; take keys from guard; give coins to vendor

The returned action phrase will act as a pattern to match on. Certain prepositions ("at", "in") will have different contexts depending on the command. "go in cave" implies the actor is moving, whereas each of "look in bag" and "look at bag" mean different things. If the preposition doesn't have significance, then the most simple action will be taken, e.g. "look bag" is same as "look at bag."

Only some of these make sense in a lightweight game. For instance, we could parse "hide behind the tree" to mean that the player is going to take that action, but there's little point. For one, there are no antagonists (at this time; a multi-player version may be viable) to seek out the player. Likewise, the act of hiding something behind a tree indicates the need for a location separate from the current space the player. So, the very least, the "hide" verb as applied to the player doesn't make much sense. Hiding objects may make sense however, as a future plan for the game is to have a moving antagonist who may react to a location depending on what is there. If a player left something behind to be found later by another actor (recall, the player will eventually be able to form severable party), then the roaming antagonist may need react --- and all this said, it may justify the hiding of an actor in way that means the player is forced to switch to another actor.

### verb

- inventory
- look
- quit

### verb preposition target

- go cave
- lock door
- go to cave
- go inside cave
- go inside of the cave
- move to cave
- move to north cave
- look into box
- look in old bag
- look old bag
- look at bag
- look towards the north

### verb object preposition target

- put bauble in bag
- put cup on tall table
- put cup on top of the table
- put the cup on the top of the table -- same as "on"
- put cup top table                   -- same as "on"
- put cup under table
- push chair under table
- drop cat from roof

# Conditional Phrase Parse Rules

These are phrases that are internal to the adventure and not typed into the command line by the player. As with verb phrases, the articles don't matter.

They one of two forms:

1. Basic: Basic phrases which establish the state of location, item, or actor
2. Tagged: Similar to Basic, except the possessive clause refers to a tag or type (type and tag are synonyms).

## Rationale

In the "scenarios" object list in an adventure JSON file, the creator will establish the baseline of each of the major scenarios. For example, there might be scenario where the guard will allow the the player to pass the castle gate only if a bribe is given _and_ the guard's commander is not watching. Essentially there will be a series of states that need to be met before a particular action can be taken. In the example, once the opening conditions are met, then the player can go into the castle through the castle gate route.

In code, this means parsing the initial scenarios from human readable form to the in-game state management. Each scenario will have a startCondition like so:

```json
"startConditions": [ {
    "all": [ "true castle entry is locked" ]
  } ],
```

which will be an object in code like this:

```haskell
-- TBD
```

In some cases, parts of the scenario are evaluated behind the scenes, preventing the player access; more often than not, the user will get some sort of feedback, clue, or hint as to what can be done to overcome an obstacle, though they may not know the entire extent of what needs to be done, based on how many conditions there are to match the end conditions.

## Phrase Forms

### Basic

- subject [negation] condition  - e.g., door locked, guard standing, guard not standing
  - negations: "no," "not"
- subject verb condition        - e.g., door is locked, queen is ruler, bauble is red
  - verbs: "is," "is not," "are," "are not"
- subject verb possession       - e.g., queen has ring, guard has coin
  - verbs" "has," "has no,", "doesn't have," "doesn't have any"

### Tagged

Negation and verbs are as with the Basic structure.

- subject [negation] condition "tag|type"   - e.g., bauble blue tag, bauble not blue type
- subject "tag|type" verb condition         - e.g., bauble tag is blue, bauble type is not blue
- subject verb condition "tag|type"         - e.g., bauble is blue tag, bauble type is blue type
- subject verb possession "tag|type"        - e.g., bauble has blue tag, bauble type has blue type
- subject verb "of tag|of type" condition   - e.g., bauble is of type blue, bauble is of tag blue

## Conditions Verbs List

- is: is, is not, not, are, are not, is at, at, not at, is not at
- has: has, has no, does not have, doesn't have, don't have

##  Examples

### Basic Examples
- castle locked
- castle is locked
- castle not locked
- castle is not locked
- castle door is not locked
- the guard has the key
- guard does not have the key
- guard has key
- guard has no key
- bauble is blue
- alice is queen
- alice is not queen

### Tagged Examples
These make use of potential tags that can be applied to Entities.

- bauble blue tag
- bauble has blue tag
- bauble has a blue tag
- bauble tag is blue
- guard is of type royal
- guard type is royal
- guard doesn't have any key -- `any` here implies a type.
