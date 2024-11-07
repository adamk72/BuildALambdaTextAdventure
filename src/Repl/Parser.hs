{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Repl.Parser (module Repl.Parser) where

import qualified Data.Text as T
import Data.Text (Text, unwords)
import Data.List (find)
import Prelude hiding (pred, unwords)

data ActionPhrase = ActionPhrase
    { verb :: Verb
    , objectNoun :: Noun
    , preposition :: Preposition
    , prepositionalNoun :: Noun
    } deriving (Show, Eq)

newtype Verb = Verb Text deriving (Show, Eq)
newtype Noun = Noun Text deriving (Show, Eq)
newtype Preposition = Preposition Text deriving (Show, Eq)

-- Known word sets
knownVerbs :: [Text]
knownVerbs = ["put", "place", "move", "set"]

knownPrepositions :: [Text]
knownPrepositions = ["in", "on", "under", "beside"]

knownArticles :: [Text]
knownArticles = ["the", "a", "an"]

isVerb :: Text -> Bool
isVerb = flip elem knownVerbs

isPreposition :: Text -> Bool
isPreposition = flip elem knownPrepositions

isArticle :: Text -> Bool
isArticle = flip elem knownArticles

-- Find the first word satisfying a predicate after a given index
findWordAfter :: (Text -> Bool) -> [Text] -> Int -> Maybe (Text, Int)
findWordAfter pred words' startIdx =
    find (pred . fst) $ zip (drop startIdx words') [startIdx..]

skipArticles :: [Text] -> Int -> Int
skipArticles words' idx
    | idx < length words' && isArticle (words' !! idx) = idx + 1
    | otherwise = idx

parseActionPhrase :: Text -> Maybe ActionPhrase
parseActionPhrase input = do
    let words' = T.words $ T.toLower input

    (verb, verbIdx) <- findWordAfter isVerb words' 0

    (prep, prepIdx) <- findWordAfter isPreposition words' (verbIdx + 1)

    let objStartIdx = skipArticles words' (verbIdx + 1)
    guard $ objStartIdx < length words'
    let obj = words' !! objStartIdx
    guard $ not $ isPreposition obj

    let prepNounStartIdx = skipArticles words' (prepIdx + 1)
    guard $ prepNounStartIdx < length words'
    let prepNoun = words' !! prepNounStartIdx

    return $ ActionPhrase
        (Verb verb)
        (Noun obj)
        (Preposition prep)
        (Noun prepNoun)
  where
    guard :: Bool -> Maybe ()
    guard True = Just ()
    guard False = Nothing

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