{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Parser.Parser (parseCmdPhrase, parseCondPhrase, renderExpression, renderExpressionError) where

import           Data.Text       (Text, unwords)
import qualified Data.Text       as T
import           Parser.Patterns
import           Parser.Types
import           Prelude         hiding (unwords, words)

isArticle :: Text -> Bool
isArticle = (`elem` knownArticles)

-- | MakeClause helper function
mkCl :: (Text -> a) -> [Text] -> a
mkCl ctor = ctor . T.unwords

findWithPattern :: [Text] -> [(a1, [[Text]])] -> MatchPreference -> Either a2 (Maybe (a1, [Text], [Text]))
findWithPattern words pat pref = Right $ findPattern pat pref words

parseCondPhrase :: Text -> Either ParseError CondExpression
parseCondPhrase input = do
    let words' = filter (not . isArticle) $ T.words $ T.toLower input
    case words' of
        [] -> Left $ MalformedCondExpression input
        _  -> runParseCondPhrase words'

runParseCondPhrase :: [Text] -> Either ParseError CondExpression
runParseCondPhrase [] = Left TBDError
runParseCondPhrase clause = do
    condResult <- findWithPattern clause knownCondPatterns First
    case condResult of
        Nothing           -> Left $ MalformedCondExpression $ unwords clause
        Just (patternType, subject, condition) ->
            case patternType of
                AtLocation    -> makeStateExpr AtLocationExpression
                NotAtLocation -> makeStateExpr NotAtLocationExpression
                Possessive    -> makePossesExpr PossessiveExpression
                NonPossessive -> makePossesExpr NonPossessiveExpression
                PosState      -> makeStateExpr PosStateExpression
                NegState      -> makeStateExpr NegStateExpression
            where
                makePossesExpr :: (SubjClause -> PossessionClause -> b) -> Either a b
                makePossesExpr ctor =
                    Right $ ctor (mkCl SubjClause subject) (mkCl PossessionClause condition)
                makeStateExpr :: (SubjClause -> StateClause -> b) -> Either a b
                makeStateExpr ctor =
                    Right $ ctor (mkCl SubjClause subject) (mkCl StateClause condition)

parseCmdPhrase :: Text -> Either ParseError CmdExpression
parseCmdPhrase input = do
    let words' = filter (not . isArticle) $ T.words $ T.toLower input
    case words' of
        []     -> Left $ MalformedCmdExpression input
        (w:ws) -> runParseCmdPhrase w ws

runParseCmdPhrase :: Text -> [Text] -> Either ParseError CmdExpression
runParseCmdPhrase verb [] = Right $ AtomicCmdExpression verb
runParseCmdPhrase verb clause = do
    prepResult <- findWithPattern clause knownPreps Last
    case prepResult of
        Nothing -> Right $ UnaryCmdExpression verb (mkCl NounClause clause)
        Just (prep, beforePrep, afterPrep) ->
            case (beforePrep, afterPrep) of
                ([], []) -> Left MissingTarget
                ([], target) ->
                    if verb `elem` verbsRequiringObjects
                    then Left MissingObject
                    else Right $ SplitCmdExpression verb
                                 (PrepClause prep)
                                 (mkCl NounClause target)
                (obj, target) ->
                    Right $ ComplexCmdExpression verb
                            (mkCl NounClause obj)
                            (PrepClause prep)
                            (mkCl NounClause target)

renderExpression :: CmdExpression -> Text
renderExpression = \case
    AtomicCmdExpression verb ->
        verb
    UnaryCmdExpression verb target ->
        T.unwords [verb, unNounClause target]
    SplitCmdExpression verb prep target ->
        T.unwords [verb, unPrepClause prep, unNounClause target]
    ComplexCmdExpression verb obj prep target ->
        T.unwords [verb, unNounClause obj, unPrepClause prep, unNounClause target]

renderExpressionError :: ParseError -> Text
renderExpressionError = \case
    TBDError -> "TBD on ths one"
    CheckingExpression phrase -> "What we have: " <> phrase
    MalformedCondExpression phrase -> "Conditional phrase is incomplete or possibly empty: " <> phrase
    MissingObject ->
        "This phrase needs an object to act on. For example: 'put bauble in bag', where 'bauble' is the object."
    MissingTarget ->
        "This phrase needs a target. For example: 'put bauble in bag' where 'bag' is the target."
    MalformedCmdExpression "" ->
        "Did you mean to type a command?"
    MalformedCmdExpression expr ->
        "I couldn't understand '" <> expr <> "'. Please try rephrasing your command."
