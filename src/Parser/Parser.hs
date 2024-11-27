module Parser.Parser (parsePhrase, renderExpression, renderExpressionError) where

import           Data.Maybe   (listToMaybe)
import           Data.Text    (Text)
import qualified Data.Text    as T
import           Parser.Types
import           Parser.Utils

-- | Helper functions
isArticle :: Text -> Bool
isArticle = (`elem` knownArticles)

makeNounClause :: [Text] -> NounClause
makeNounClause = NounClause . T.unwords

findPrepClause :: [Text] -> Either ParseError (Maybe (Text, [Text], [Text]))
findPrepClause words' = Right $ listToMaybe $ reverse
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
parsePhrase :: Text -> Either ParseError Expression
parsePhrase input = do
    let words' = filter (not . isArticle) $ T.words $ T.toLower input
    case words' of
        []     -> Left $ MalformedExpression input
        (w:ws) -> parsePhraseType w ws

parsePhraseType :: Text -> [Text] -> Either ParseError Expression
parsePhraseType verb [] = Right $ AtomicExpression verb
parsePhraseType verb words' = do
    prepResult <- findPrepClause words'
    case prepResult of
        Nothing -> Right $ UnaryExpression verb (makeNounClause words')
        Just (prep, beforePrep, afterPrep) ->
            case (beforePrep, afterPrep) of
                ([], []) -> Left MissingTarget
                ([], target) ->
                    if verb `elem` verbsRequiringObjects
                    then Left MissingObject
                    else Right $ BinaryExpression verb
                                 (PrepClause prep)
                                 (makeNounClause target)
                (obj, target) ->
                    Right $ ComplexExpression verb
                            (makeNounClause obj)
                            (PrepClause prep)
                            (makeNounClause target)

-- | Rendering functions
renderExpression :: Expression -> Text
renderExpression = \case
    AtomicExpression verb ->
        verb
    UnaryExpression verb target ->
        T.unwords [verb, unNounClause target]
    BinaryExpression verb prep target ->
        T.unwords [verb, unPrepClause prep, unNounClause target]
    ComplexExpression verb obj prep target ->
        T.unwords [verb, unNounClause obj, unPrepClause prep, unNounClause target]

renderExpressionError :: ParseError -> Text
renderExpressionError = \case
    MissingObject ->
        "This phrase needs an object to act on. For example: 'put bauble in bag', where 'bauble' is the object."
    MissingTarget ->
        "This phrase needs a target. For example: 'put bauble in bag' where 'bag' is the target."
    MalformedExpression "" ->
        "Did you mean to type a command?"
    MalformedExpression expr ->
        "I couldn't understand '" <> expr <> "'. Please try rephrasing your command."
