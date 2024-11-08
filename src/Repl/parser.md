# Command patterns

# Parse rules

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

## verb

- inventory
- look
- quit

## verb preposition target

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

## verb object preposition target

- put bauble in bag
- put cup on tall table
- put cup on top of the table
- put the cup on the top of the table -- same as "on"
- put cup top table                   -- same as "on"
- put cup under table
- push chair under table
- drop cat from roof




