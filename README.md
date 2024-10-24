# TextAdventure

## Getting it running:

- `stack build`
- `stack run TextAdventure-exe -- --help`

## Todos

### Doc/Blog Todos

- [] Go over `mapM` and `mapM_` in context of reading from a file directory and printing out the file names: `listDirectory storyDirectory >>= mapM_ putStrLn`. See Listing 22.3 in the GPWH book.

### Coding Todos

- [x] Get basic launch structure into place

### Super Advanced Coding Todos

- [] Use [Parsec](https://hackage.haskell.org/package/parsec) for the REPL, per [this](https://blogg.bekk.no/creating-a-repl-in-haskell-efcdef1deec2) article's suggestion. It's an older article, so follow up on other options later.
