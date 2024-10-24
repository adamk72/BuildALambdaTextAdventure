{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module PrintUtils where

import           System.IO             (hFlush, stdout)

import qualified Data.Char
import qualified Data.List.Split
import           NarrativeGraph        (Flags (..), Inventory (..))
import           NaturalLanguageLexer  (Prepositions, Token (..),
                                        TokenMatch (..), Tokens, Verbs,
                                        lexInput)
import           NaturalLanguageParser (Sentence, parseSentence)
import           PrintParams           (allDelimiters)
import           TextReflow            (reflowPutStr, reflowPutStrs)


printTokens :: String -> [Token] -> IO ()
printTokens word [] = putStr "\n" >> hFlush stdout
printTokens word ((TokenVerb _ _) : tokens) = putStrLn ("== Verb " ++ word) >> printTokens word tokens >> hFlush stdout
printTokens word ((TokenNoun _ _) : tokens) = putStrLn ("== Noun " ++ word) >> printTokens word tokens >> hFlush stdout
printTokens word ((TokenPreposition _ _) : tokens) = putStrLn ("== Preposition " ++ word) >> printTokens word tokens >> hFlush stdout

--Print tokens for a word
printWordTokens :: [TokenMatch] -> IO ()
printWordTokens [] = putStr "\n" >> hFlush stdout
printWordTokens ((TokenMatch word matchedTokens) : tokens) = printTokens word matchedTokens >> printWordTokens tokens >> hFlush stdout

--Print sentence
printSentences :: [Sentence] -> IO ()
printSentences [] = putStr "I'm sorry, I don't understand what you said." >> hFlush stdout
printSentences [sentence] = print sentence >> hFlush stdout
printSentences (sentence : sentences) = print sentence >> printSentences sentences >> hFlush stdout

--Print intro
printIntro :: IO ()
printIntro
    = reflowPutStrs ["Haskell Text Adventure Engine v1.0\n", "Copyright Laurence Emms 2018\n"] >>
      hFlush stdout

--Print help text
printHelp :: IO ()
printHelp
    = reflowPutStrs ("The following commands are available:\n" :
                    fmap (\t -> "\t> "++t++"\n")
                        ["Inventory - Print all current inventory items.",
                        "Help - Print help text.",
                        "Grammar - Print available grammar.",
                        "Verbs - Print all available verbs.",
                        "Prepositions - Print all available prepositions.",
                        -- "Nouns - Print all available nouns. Warning, this contains spoilers!",
                        -- "Flags - Print all current flags. Warning, this contains spoilers!",
                        "Quit - Exit the  game."
                        ]
                    ++ ["------------------"]
                    )
      >> hFlush stdout

printGrammar :: IO ()
printGrammar
    = reflowPutStrs ["Simple sentence: <Verb> <Noun>\n",
                     "Simple preposition sentence: <Verb> <Preposition> <Noun>\n",
                     "Complex sentence: <Verb> <Noun> <Preposition> <Noun>\n",
                     "Complex preposition sentence: <Verb> <Preposition> <Noun> <Preposition> <Noun>\n"] >>
      hFlush stdout

printVerbs :: [Token] -> IO ()
printVerbs [] = putStr "\n" >> hFlush stdout
printVerbs ((TokenVerb name synonyms) : tokens)
    = reflowPutStr ("Synonyms for " ++ name ++ ": " ++ show synonyms ++ "\n") >>
      printVerbs tokens >>
      hFlush stdout
printVerbs (_ : tokens)
    = reflowPutStr "Invalid token\n" >>
      printVerbs tokens >>
      hFlush stdout

printNouns :: [Token] -> IO ()
printNouns [] = putStr "\n" >> hFlush stdout
printNouns ((TokenNoun name synonyms) : tokens)
    = reflowPutStr ("Synonyms for " ++ name ++ ": " ++ show synonyms ++ "\n") >>
      printNouns tokens >> hFlush stdout
printNouns (_ : tokens)
    = reflowPutStr "Invalid token\n" >>
      printNouns tokens >> hFlush stdout

printPrepositions :: [Token] -> IO ()
printPrepositions [] = putStr "\n" >> hFlush stdout
printPrepositions ((TokenPreposition name synonyms) : tokens)
    = reflowPutStr ("Synonyms for " ++ name ++ ": " ++ show synonyms ++ "\n") >>
      printPrepositions tokens >> hFlush stdout
printPrepositions (_ : tokens)
    = reflowPutStr "Invalid token\n" >>
      printPrepositions tokens >> hFlush stdout

printInventory :: Inventory -> IO ()
printInventory (Inventory []) = putStr "\n" >> hFlush stdout
printInventory (Inventory (object : remainingInventory))
    = reflowPutStr (object ++ "\n") >>
      printInventory (Inventory remainingInventory) >> hFlush stdout

printFlags :: Flags -> IO ()
printFlags (Flags []) = putStr "\n" >> hFlush stdout
printFlags (Flags (flag : remainingFlags))
    = reflowPutStr (flag ++ ".\n") >>
      printFlags (Flags remainingFlags) >> hFlush stdout

parseInput :: Verbs -> Prepositions -> Tokens -> Inventory -> Flags -> String -> IO (Maybe [Sentence])
parseInput verbs prepositions tokens inventory flags line
    | isCmd line "help" = printHelp >> return (Just [])
    | isCmd line "grammar" = putStrLn "All grammar:" >> printGrammar >> return (Just [])
    | isCmd line "verbs" = putStrLn "All verbs:" >> printVerbs verbs >> return (Just [])
    -- | isCmd line "nouns" = putStrLn "All nouns:" >> printNouns allNouns >> return (Just [])
    | isCmd line "prepositions" = putStrLn "All prepositions:" >> printPrepositions prepositions >> return (Just [])
    | isCmd line "inventory" = putStrLn "All items in inventory:" >> printInventory inventory >> return (Just [])
    -- | isCmd line "flags" = putStrLn "All currently set flags:" >> printFlags flags >> return (Just [])
    | isCmd line ":q" || isCmd line "quit" || isCmd line ":quit" = putStrLn "Thanks for playing!" >> hFlush stdout >> return Nothing
    | null sentences = putStr "I'm sorry, I don't understand what you said." >> hFlush stdout >> return (Just sentences)
    | otherwise = --printWordTokens sentenceTokenMatches >>
                  --printSentences sentences >>
                  hFlush stdout >>
                  return (Just sentences)
        where inputWords = Data.List.Split.splitOneOf allDelimiters line
              sentenceTokenMatches = lexInput tokens inputWords
              sentences = parseSentence sentenceTokenMatches
              isCmd a b = map Data.Char.toLower a == b
