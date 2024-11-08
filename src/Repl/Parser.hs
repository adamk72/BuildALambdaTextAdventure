{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Repl.Parser
    ( parseExpression
    , Expression(..)
    , Prep(..)
    , Phrase(..)
    , ParseError(..)
    , renderExpression
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