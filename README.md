# TextAdventure

## Getting it running:

- `stack build`
- `stack run TextAdventure-exe -- --help`

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

- [x] Get basic launch structure into place
- [x] Abstract things like the ":quit" command or "REPL> " prompt out of the code; what if I want to easily change or i18n those words?
- [x] On launch, the command selections should be dynamic, based on what is pulled from the JSON files.
- [] Convert the old code over from String to Text only.
- [] Build a naive spell checker for checking input and making suggestions.
- [] Import world from JSON file.
  1. [] Just get a simple gameWorld or player structure in place at launch.
  2. [] Structure the data.
  3. [] Pull in data from file.
- [] Create move command from Meadow to Cave and back.
- [] Have REPL prompt dynamically update when the location (or other details) change.


### Super Advanced Coding Todos

- [] Use [Parsec](https://hackage.haskell.org/package/parsec) for the REPL, per [this](https://blogg.bekk.no/creating-a-repl-in-haskell-efcdef1deec2) article's suggestion. It's an older article, so follow up on other options later.
