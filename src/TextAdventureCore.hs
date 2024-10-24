module TextAdventureCore (doAdventureLoop, updateAdventure, adventure) where

import           System.IO             (hFlush, stdout)

import           NarrativeGraph        (Flags, Inventory, NarrativeGraph,
                                        SceneKey, performInteraction,
                                        printInvalidInteractions,
                                        printSceneDescription)
import           NaturalLanguageLexer
import           NaturalLanguageParser (Sentence)
import           PrintUtils            (parseInput)
import           TextReflow            (reflowPutStr)

doAdventureLoop :: Verbs -> Prepositions -> Tokens -> NarrativeGraph -> SceneKey -> Inventory -> Flags -> Maybe [Sentence] -> IO (Maybe (SceneKey, Inventory, Flags))
doAdventureLoop _ _ _ _ _ _ _ Nothing = return Nothing -- End state of the game
doAdventureLoop verbs prepositions tokens narrativeGraph sceneKey inventory flags (Just []) = adventure verbs prepositions tokens narrativeGraph (Just (sceneKey, inventory, flags)) --Failed to parse any sentences
doAdventureLoop verbs prepositions tokens narrativeGraph sceneKey inventory flags (Just sentences) = performInteraction narrativeGraph sceneKey inventory flags sentences >>=
    adventure verbs prepositions tokens narrativeGraph --Perform the adventure loop

updateAdventure :: Verbs -> Prepositions -> Tokens -> NarrativeGraph -> Maybe (SceneKey, Inventory, Flags) -> IO (Maybe (SceneKey, Inventory, Flags))
updateAdventure _ _ _ _ Nothing = return Nothing
updateAdventure verbs prepositions tokens narrativeGraph (Just (sceneKey, inventory, flags))
    = putStr "\n> " >>
      hFlush stdout >>
      printInvalidInteractions narrativeGraph sceneKey >>
      getLine >>=
      parseInput verbs prepositions tokens inventory flags >>=
      (\state -> putStr "\n" >> hFlush stdout >> return state) >>=
      doAdventureLoop verbs prepositions tokens narrativeGraph sceneKey inventory flags

adventure :: Verbs -> Prepositions -> Tokens -> NarrativeGraph -> Maybe (SceneKey, Inventory, Flags) -> IO (Maybe (SceneKey, Inventory, Flags))
adventure _ _ _ _ Nothing = reflowPutStr "Game over. Thanks for playing!" >> hFlush stdout >> return Nothing
adventure verbs prepositions tokens narrativeGraph (Just (sceneKey, inventory, flags)) =
    printSceneDescription
      narrativeGraph
      (Just (sceneKey, inventory, flags)) >>=
      updateAdventure verbs prepositions tokens narrativeGraph
