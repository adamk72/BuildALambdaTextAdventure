module Parser.Parser (parseCmdPhrase, parseCondPhrase, renderExpression, renderExpressionError) where

import           Data.Text                (Text)
import qualified Data.Text                as T
import           Parser.Internal.Patterns
    (MatchPreference (First, Last), findPattern, isCondTypeOf, knownArticles,knownCondPatterns,
    knownPreps, verbsRequiringObjects)
import           Parser.Types
import           Prelude                  hiding (words)

-- | Helper functions
isArticle :: Text -> Bool
isArticle = (`elem` knownArticles)

makeNounClause :: [Text] -> NounClause
makeNounClause = NounClause . T.unwords

makeSubjClause:: [Text] -> SubjClause
makeSubjClause = SubjClause . T.unwords

makeStateClause:: [Text] -> StateClause
makeStateClause = StateClause . T.unwords

makePossessionClause:: [Text] -> PossessionClause
makePossessionClause = PossessionClause . T.unwords

findPrepClause :: [Text] -> Either ParseError (Maybe (Text, [Text], [Text]))
findPrepClause words = Right $ findPattern knownPreps Last words

findCondPattern :: [Text] -> Either ParseError (Maybe (Text, [Text], [Text]))
findCondPattern words = Right $ findPattern knownCondPatterns First words

-- | Main parsing functions
parseCondPhrase :: Text -> Either ParseError CondExpression
parseCondPhrase input = do
    let words' = filter (not . isArticle) $ T.words $ T.toLower input
    case words' of
        [] -> Left MalformedCondExpression
        _  -> runParseCondPhrase words'

runParseCondPhrase :: [Text] -> Either ParseError CondExpression
runParseCondPhrase [] = Left TBDError
runParseCondPhrase clause = do
    condResult <- findCondPattern clause
    case condResult of
        Nothing           -> Left MalformedCondExpression
        Just (verbClause, subject, condition) ->
            case isCondTypeOf verbClause of
            PosState               -> makeStateExpr PosStateExpression
            NegState               -> makeStateExpr NegStateExpression
            Possessive             -> makePossesExpr PossessiveExpression
            NonPossessive          -> makePossesExpr NonPossessiveExpression
            UnknownConditionalType -> Left TBDError
            where
                makePossesExpr ctor =
                    Right $ ctor (makeSubjClause subject) (makePossessionClause condition)
                makeStateExpr ctor =
                    Right $ ctor (makeSubjClause subject) (makeStateClause condition)

parseCmdPhrase :: Text -> Either ParseError CmdExpression
parseCmdPhrase input = do
    let words' = filter (not . isArticle) $ T.words $ T.toLower input
    case words' of
        []     -> Left $ MalformedCmdExpression input
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
    MalformedCondExpression -> "Phrase is incomplete."
    MissingObject ->
        "This phrase needs an object to act on. For example: 'put bauble in bag', where 'bauble' is the object."
    MissingTarget ->
        "This phrase needs a target. For example: 'put bauble in bag' where 'bag' is the target."
    MalformedCmdExpression "" ->
        "Did you mean to type a command?"
    MalformedCmdExpression expr ->
        "I couldn't understand '" <> expr <> "'. Please try rephrasing your command."
