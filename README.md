# TextAdventure

## Getting it running:

- `stack build`
- `stack run TextAdventure-exe -- --help`

## How to test

- `stack test`
- `stack test --test-arguments "--match=Look"` to run an individual test (that matches the word "Look" in this case)

## Using the converter

This was AI generated; using it for the moment so I can quickly create variations of test worlds.

- `stack repl`
- `ghci> GameConfigConverter.main`
  - input the source file path, e.g., "stories/TrialAdventure.json"
  - input the output directory, e.g., "test/Mock"

Use in code or have it run the tests, though at the moment, they are nominal.

## GameWorld

```
GameWorld {activeCharacter = Character {charTag = "alice", charName = "Alice the Adventurer", currentLocation = Location {locTag = "meadow", locName = "The Meadow"}}, playableCharacters = [Character {charTag = "bob", charName = "Bob the Brave", currentLocation = Location {locTag = "cave", locName = "The Cave"}}], locations = [Location {locTag = "meadow", locName = "The Meadow"},Location {locTag = "cave", locName = "The Cave"}]}
```

## Game Ideas

- Allow the player to have multiple characters in multiple location:
  - Have a command that lets the player switch between the characters;
  - If in the same location, allow direct communication and exchange of goods;
  - If in different locations, certain tools would allow for commands to be given from one character to another.
  - The thought here is that characters could act as distractions or provide "simultaneous" actions to occur that may impact the story arc.

## Todos

### Cleanup Todos
[] Fix exports to only what's necessary.
[] Rename variables and functions to be more consistent.
[] Clean up tests; try to simplify.
[] Create a test command history file with expected outcomes to check against.
[] Standardize all of the messages/errors, especially ones that are in situ.
[] Rename the app (get rid of "my").
[] Clean up this documentation; move the "todos" to their own file and the blog ideas to the blog.

### Coding Todos
- [] Supplementary for scenarios
  - [x] Create "give" command for NPC interaction.
  - [] Create "talk" command for NPC interaction.
  - [x] "Look" should describe NPCs in location
  - [x] "Look" should describe locations exits;
  - [] For Look, not all exits will be "real" and lead to other places.
  - [] Allow for "open" and "close" commands (is this necessary; can we assume that if something isn't locked, it can be opened without needing a specific command?) Maybe treating as a status, like, "blocked" and "unblocked" makes more sense.
  - [] Allow for "lock" and "unlock" command.
  - [] Possibly add "types" to Entities in order to generalize them. This way when using the "guard" tag, it means any guard instead of specific guard (with possibility of having subtypes like "bribable"?).
  - [] Work out how currency and commodities will work so the player can hold and give arbitrary amounts of some items.
- [] Scenario Work
  - [] Do the supplementary work, above.
  - [x] Aeson parse the scenarios section of the Trial Adventure.json file.
  - [x] Determine how you're actually going to access that data in the midst of a command use.
- [] Logging, Gave saves, Game replays
  - [x] Log status for debugging.
  - [] Where there are `error` throws, try logging instead.
  - [x] Save current state of game to files.
  - [] Save commands so that the user can up arrow to get last command.
  - [] Save (valid) moves to files so that the game can be replayed for testing.
- [] Create 'prospect' location status, indicating Look is misdirecting the player
  - [] Player should be able to have a running memory of what place has been visited, lifting the "prospect" condition.
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
- [x] Create move command from Meadow to Cave and back.
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

### Doc/Blog Todos

- [] Go over `mapM` and `mapM_` in context of reading from a file directory and printing out the file names: `listDirectory storyDirectory >>= mapM_ putStrLn`. See Listing 22.3 in the GPWH book.
- []  Todo: blog post on guard: guard :: Alternative f => Bool -> f ()
- [] (<|>) is the alternative operator from the Alternative typeclass in Haskell. For Maybe values, it acts like an "or" operation - it returns the first Just value it finds, or Nothing if both options are Nothing.
- [] Discuss common file pattern in Haskell where:
  - Core types are in a .Types module
  - Implementation details are in separate modules
  - A main module re-exports everything needed
- [] For later comparison on how `case` is very flexible:
```haskell
RunAdventure name -> do
    let matchingAdventure = filter (\adv -> advLaunchTag adv == unAdventureName name) adventures
    case matchingAdventure of
        [adv] -> do -- [adv] is used for pattern matching a list with one element only
            runGameWithOption (pack $ filePath adv)  -- Pass the file path instead of the launch tag
        _ -> TIO.putStrLn "Error: Adventure not found or multiple matches found"
```
- [] Aeson things:
  - [] sensitivity of this:
```haskell
instance FromJSON Location where
    parseJSON = withObject "Location" $ \v -> do
        locTag <- v .: "tag"
        locName <- v .: "name"
        destinationTags <- v .: "destinationTags"
        return Location{..}

-- Sensitive to field order:
{-
instance FromJSON Location where
    parseJSON = withObject "Location" $ \v ->
        Location
            <$> v .: "tag"
            <*> v .: "name"
            <*> v .: "destinationTags"
-}
```

- [] Comparison of all Repl.hs code

``` haskell
{- This is WAY better
  maybe :: b -> (a -> b) -> Maybe a -> b
The maybe function takes a default value, a function, and a Maybe value. If the Maybe value is Nothing, the function returns the default value. Otherwise, it applies the function to the value inside the Just and returns the result.
-}
{- Previous code for comparison:
  case outM of
    Just out -> do
        print_ out
        return (Just st)
    Nothing -> return Nothing
-}
-- Just st <$ mapM_ print_ outM -- why doesn't this work?
```

```haskell
    -- Todo: Talk about init and last in blog
    let init' = init xs
        last' = last xs
        commaList = intercalate ", " (map toLower init')
```

## Trigger patterns

```haskell
-- Todo: Note this as a trigger pattern
{- Previous version for comparison
launch :: FilePath -> IO ()
launch fp = do
   geJSON <- loadGameEnvironmentJSON fp
   case geJSON of
    Right adventure -> do
        print $ world adventure
        gameLoop $ world adventure
    Left e -> print e
-}
```

```haskell
let validPaths = Either.rights filePaths
mapM processFile validPaths

-- Todo: add to trigger list (Redundant Return; mapM)
let validPaths = Either.rights filePaths -- filter out the bad files
results <- mapM processFile validPaths
return $ results
```

