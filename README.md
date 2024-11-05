# TextAdventure

## Getting it running:

- `stack build`
- `stack run TextAdventure-exe -- --help`

## How to test

- `stack test`
- `stack test --test-arguments "--match=Look"` to run an individual test (that matches the word "Look" in this case)

## GameWorld

```
GameWorld {activeCharacter = Character {charTag = "alice", charName = "Alice the Adventurer", currentLocation = Location {locTag = "meadow", locName = "The Meadow"}}, playableCharacters = [Character {charTag = "bob", charName = "Bob the Brave", currentLocation = Location {locTag = "cave", locName = "The Cave"}}], locations = [Location {locTag = "meadow", locName = "The Meadow"},Location {locTag = "cave", locName = "The Cave"}]}
```

## Game Ideas

- Allow the player to have multiple characters in multiple location:
  - Have a command that let's the player switch between the characters;
  - If in the same location, allow direct communication and exchange of goods;
  - If in different locations, certain tools would allow for commands to be given from one character to another.
  - The thought here is that characters could act as distractions or provide "simultaneous" actions to occur that may impact the story arc.

## Todos

### Doc/Blog Todos

- [] Go over `mapM` and `mapM_` in context of reading from a file directory and printing out the file names: `listDirectory storyDirectory >>= mapM_ putStrLn`. See Listing 22.3 in the GPWH book.

### Coding Todos

- [] Determine if it's worth consolidating the concept of "interactables" with that of "locations," since the code might end up being very similar (same with "characters").
- [x] Properly update the Character's location when moving.
- [x] Get basic launch structure into place.
- [x] Abstract things like the ":quit" command or "REPL> " prompt out of the code; what if I want to easily change or i18n those words?
- [x] On launch, the command selections should be dynamic, based on what is pulled from the JSON files.
- [x] Convert the old code over from String to Text only.
- [] Build a naive spell checker for checking input and making suggestions.
- [x] Import world from JSON file.
  1. [x] Just get a simple gameWorld or player structure in place at launch.
  2. [x] Structure the data.
  3. [x] Pull in data from file.
- [] Create move command from Meadow to Cave and back.
- [x] Move Quit commands over to the parser.
  - [x] Fix it so that it returns a proper Maybe or Either rather than a Text.
- [] Have REPL prompt dynamically update when the location (or other details) change.
- [x] Add proper testing back in since I'm learning from AI but results need to be confirmed.
  - [x] Figure out to handle :q command.
  - [x] Learn how to set up pre-test config to eliminate the redundant use of the same processConfig arguments.
  - [x] Figure out how to put dummy instances of the missing pattern matching so I can get rid of the OPTIONS_GHC warning which is currently suppressed.
  - [x] Abstract out the hard-coded strings (ongoing).
- [] Determine if QuickCheck will be useful in this context. (https://hspec.github.io/quickcheck.html)
- [] Validate the JSON file
  - [] Validate that the destinationTags lists contain only tags that have "full" membership in the `locations` field.

## Coding Checks

- [] I think I don't need the commands to return a Maybe.

### Super Advanced Coding Todos

- [] Use [Parsec](https://hackage.haskell.org/package/parsec) for the REPL, per [this](https://blogg.bekk.no/creating-a-repl-in-haskell-efcdef1deec2) article's suggestion. It's an older article, so follow up on other options later.
