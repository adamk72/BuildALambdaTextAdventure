module Parser.Parser (parseCmdPhrase, renderExpression, renderExpressionError) where

import           Command.CommandInfo      (knownCmdVerbs)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Parser.Internal.Patterns
    (MatchPreference (First, Last), findPattern, knownArticles, knownPatterns, knownPreps, verbsRequiringObjects)
import           Parser.Types
import           Prelude                  hiding (words)

-- | Helper functions
isArticle :: Text -> Bool
isArticle = (`elem` knownArticles)

makeNounClause :: [Text] -> NounClause
makeNounClause = NounClause . T.unwords

makeCondClause :: [Text] -> CondClause
makeCondClause = CondClause . T.unwords

findPrepClause :: [Text] -> Either ParseError (Maybe (Text, [Text], [Text]))
findPrepClause words = Right $ findPattern knownPreps Last words

findCondPattern :: [Text] -> Either ParseError (Maybe (Text, [Text], [Text]))
findCondPattern words = Right $ findPattern knownPatterns First words

-- | Main parsing functions
parseCondPhrase :: Text -> Either ParseError CondExpression
parseCondPhrase input = do
    let words' = filter (not . isArticle) $ T.words $ T.toLower input
    case words' of
        []     -> Left $ MalformedExpression input
        (w:ws) -> runParseCondPhrase w ws

runParseCondPhrase :: Text -> [Text] -> Either ParseError CondExpression
runParseCondPhrase _ [] = Left TBDError
runParseCondPhrase subject clause = do
    condResult <- findCondPattern clause
    case condResult of
        Nothing           -> Right $ UnaryCondExpression subject (makeCondClause clause)
        Just (t0, t1, t2) -> undefined

parseCmdPhrase :: Text -> Either ParseError CmdExpression
parseCmdPhrase input = do
    let words' = filter (not . isArticle) $ T.words $ T.toLower input
    case words' of
        []     -> Left $ MalformedExpression input
        (w:ws) -> runParseCmdPhrase w ws

runParseCmdPhrase :: Text -> [Text] -> Either ParseError CmdExpression
runParseCmdPhrase verb [] = Right $ AtomicCmdExpression verb
runParseCmdPhrase verb clause = do
    prepResult <- findPrepClause clause
    case prepResult of
        Nothing -> Right $ UnaryCmdExpression verb (makeNounClause clause)
        Just (prep, beforePrep, afterPrep) ->
            case (beforePrep, afterPrep) of
                ([], []) -> Left MissingTarget
                ([], target) ->
                    if verb `elem` verbsRequiringObjects
                    then Left MissingObject
                    else Right $ BinaryCmdExpression verb
                                 (PrepClause prep)
                                 (makeNounClause target)
                (obj, target) ->
                    Right $ ComplexCmdExpression verb
                            (makeNounClause obj)
                            (PrepClause prep)
                            (makeNounClause target)

-- | Rendering functions
renderExpression :: CmdExpression -> Text
renderExpression = \case
    AtomicCmdExpression verb ->
        verb
    UnaryCmdExpression verb target ->
        T.unwords [verb, unNounClause target]
    BinaryCmdExpression verb prep target ->
        T.unwords [verb, unPrepClause prep, unNounClause target]
    ComplexCmdExpression verb obj prep target ->
        T.unwords [verb, unNounClause obj, unPrepClause prep, unNounClause target]

renderExpressionError :: ParseError -> Text
renderExpressionError = \case
    TBDError -> "TBD on ths one"
    MissingObject ->
        "This phrase needs an object to act on. For example: 'put bauble in bag', where 'bauble' is the object."
    MissingTarget ->
        "This phrase needs a target. For example: 'put bauble in bag' where 'bag' is the target."
    MalformedExpression "" ->
        "Did you mean to type a command?"
    MalformedExpression expr ->
        "I couldn't understand '" <> expr <> "'. Please try rephrasing your command."
