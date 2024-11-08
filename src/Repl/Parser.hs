{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Repl.Parser (module Repl.Parser) where

import qualified Data.Text as T
import Data.Text (Text, unwords)
import Data.List (find)
import Command.Definitions
import Prelude hiding (pred, unwords)
import Utils

data ActionPhrase = ActionPhrase
    { verb :: Verb
    , objectNoun :: Noun
    , preposition :: Preposition
    , prepositionalNoun :: Noun
    } deriving (Show, Eq)

newtype Verb = Verb Text deriving (Show, Eq)
newtype Noun = Noun Text deriving (Show, Eq)
newtype Preposition = Preposition Text deriving (Show, Eq)

knownPrepositions :: [Text]
knownPrepositions = ["in", "on", "under", "beside", "to"]

knownArticles :: [Text]
knownArticles = ["the", "a", "an"]

isVerb :: Text -> Bool
isVerb = flip elem knownVerbs

isPreposition :: Text -> Bool
isPreposition = flip elem knownPrepositions

isArticle :: Text -> Bool
isArticle = flip elem knownArticles

findWordAfter :: (Text -> Bool) -> [(Text, Int)] -> Int -> Maybe (Text, Int)
findWordAfter pred zippedWords startIdx =
    find (pred . fst) $ dropWhile ((< startIdx) . snd) zippedWords

skipArticles :: [Text] -> Int -> Int
skipArticles words' idx
    | idx < length words' && isArticle (words' !! idx) = idx + 1
    | otherwise = idx

type Index = Int

peek :: [Text] -> Index -> Maybe Text
peek wordList idx = atMay wordList (idx + 1)

parseActionPhrase :: Text -> Maybe ActionPhrase
parseActionPhrase input = do
    let words' = zip (T.words $ T.toLower input) [0..]

    (verb, verbIdx) <- findWordAfter isVerb words' 0


    let obj = "object"
        prep = intToText verbIdx
        pn = "target"
    -- (prep, prepIdx) <- findWordAfter isPreposition words' (verbIdx + 1)

    -- let objStartIdx = skipArticles words' (verbIdx + 1)
    -- guard $ objStartIdx < length words'
    -- let obj = words' !! objStartIdx

    -- let pnIdx = skipArticles words' (prepIdx + 1)
    -- guard $ pnIdx < length words'
    -- let pn = words' !! pnIdx

    return $ ActionPhrase
        (Verb verb)
        (Noun obj)
        (Preposition prep)
        (Noun pn)
  where
    guard :: Bool -> Maybe ()
    guard True = Just ()
    guard False = Nothing

getAll :: ActionPhrase -> Text
getAll (ActionPhrase (Verb v) (Noun o) (Preposition p) (Noun pn) ) =  unwords [v, o, p, pn]

getRest :: ActionPhrase -> Text
getRest (ActionPhrase _ (Noun o) (Preposition p) (Noun pn) ) =  unwords [o, p, pn]

getVerb :: ActionPhrase -> Text
getVerb (ActionPhrase (Verb v) _ _ _) = v

getObject :: ActionPhrase -> Text
getObject (ActionPhrase _ (Noun obj) _ _) = obj

getPreposition :: ActionPhrase -> Text
getPreposition (ActionPhrase _ _ (Preposition p) _) = p

getPrepNoun :: ActionPhrase -> Text
getPrepNoun (ActionPhrase _ _ _ (Noun pn)) = pn