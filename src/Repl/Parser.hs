{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Repl.Parser
    ( parseExpression 
    , renderExpressionError
    , renderExpression
    , Expression(..)
    , Prep(..)
    , Phrase(..)
    , ParseError(..)
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (listToMaybe)
import Command.Definitions (knownVerbs)

newtype Phrase = Phrase { unPhrase :: Text }
    deriving (Show, Eq)

newtype Prep = Prep { unPrep :: Text }
    deriving (Show, Eq)

data Expression =
      AtomicExpression Text
    | UnaryExpression Text Phrase
    | PrepExpression Text Prep Phrase
    | ComplexExpression Text Phrase Prep Phrase
    deriving (Show, Eq)

data ParseError =
      UnknownVerb Text
    | MissingObject
    | MissingTarget
    | InvalidPrep
    | MalformedExpression Text
    deriving (Show, Eq)

-- | Support variables
verbsRequiringObjects :: [Text]
verbsRequiringObjects = ["put", "place", "move"]

knownPreps :: [(Text, [[Text]])]
knownPreps =
    [ ("in", [["in"], ["inside"], ["into"]])
    , ("on", [["on"], ["onto"], ["upon"], ["on", "top", "of"]])
    , ("under", [["under"], ["beneath"], ["underneath"]])
    , ("at", [["at"]])
    , ("from", [["from"]])
    , ("to", [["to"], ["toward"], ["towards"]])
    ]

knownArticles :: [Text]
knownArticles = ["the", "a", "an"]

-- | Helper functions
isArticle :: Text -> Bool
isArticle = (`elem` knownArticles)

makePhrase :: [Text] -> Phrase
makePhrase = Phrase . T.unwords

findLongestPrep :: [Text] -> Maybe (Text, [Text], [Text])
findLongestPrep words' = listToMaybe $ reverse
    [ (basePrep, before, drop (length variant) after)
    | i <- [0..length words' - 1]
    , let (before, after) = splitAt i words'
    , not (null after)
    , (basePrep, variants) <- knownPreps
    , variant <- variants
    , length variant <= length after
    , take (length variant) after == variant
    ]

-- | Main parsing functions
parseExpression :: Text -> Either ParseError Expression
parseExpression input = do
    let words' = filter (not . isArticle) $ T.words $ T.toLower input
    case words' of
        [] -> Left $ MalformedExpression input
        (w:ws) -> if w `elem` knownVerbs
                  then parseExpressionType w ws
                  else Left $ UnknownVerb w

parseExpressionType :: Text -> [Text] -> Either ParseError Expression
parseExpressionType verb [] = Right $ AtomicExpression verb
parseExpressionType verb words' =
    case findLongestPrep words' of
        Nothing -> Right $ UnaryExpression verb (makePhrase words')

        Just (prep, beforePrep, afterPrep) ->
            case (beforePrep, afterPrep) of
                ([], []) -> Left MissingTarget
                ([], target) ->
                    if verb `elem` verbsRequiringObjects -- verbs that require objects
                    then Left MissingObject
                    else Right $ PrepExpression verb
                                              (Prep prep)
                                              (makePhrase target)
                (obj, target) -> Right $ ComplexExpression verb
                                                         (makePhrase obj)
                                                         (Prep prep)
                                                         (makePhrase target)
-- | Rendering convenience functions
renderExpression :: Expression -> Text
renderExpression = \case
    AtomicExpression verb ->
        verb
    UnaryExpression verb target ->
        T.unwords [verb, unPhrase target]
    PrepExpression verb prep target ->
        T.unwords [verb, unPrep prep, unPhrase target]
    ComplexExpression verb obj prep target ->
        T.unwords [verb, unPhrase obj, unPrep prep, unPhrase target]

renderExpressionError :: ParseError -> Text
renderExpressionError = \case
    UnknownVerb verb ->
        "I don't understand the command '" <> verb <> "'. Valid commands start with: " <>
        T.intercalate ", " knownVerbs <> "."
    MissingObject ->
        "This command needs an object to act on. For example: 'get key' or 'drop sword'."
    MissingTarget ->
        "This command needs a target. For example: 'go cave' or 'look at chest'."
    InvalidPrep ->
        "Invalid preposition. Try using: in, on, under, at, from, or to."
    MalformedExpression expr ->
        "I couldn't understand '" <> expr <> "'. Please try rephrasing your command."
