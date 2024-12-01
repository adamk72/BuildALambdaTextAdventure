module Parser.Internal.Patterns
    ( MatchPreference (..)
    , PatternList
    , PatternMatch
    , findPattern
    , isCondTypeOf
    , knownArticles
    , knownCondNegations
    , knownCondPatterns
    , knownPreps
    , verbsRequiringObjects
    ) where

import           Data.Maybe   (listToMaybe, mapMaybe)
import           Data.Text    (Text, isInfixOf, toLower, unwords)
import           Parser.Types (ConditionalType (..))
import           Prelude      hiding (words)

-- | Type alias for pattern matching results
-- (matched base pattern, words before, words after)
type PatternMatch = (Text, [Text], [Text])

-- | Type alias for pattern definition lists
-- [(base pattern, list of variant word lists)]
type PatternList = [(Text, [[Text]])]

-- | Whether to prefer first or last match when multiple patterns match
data MatchPreference = First | Last
    deriving (Eq, Show)

-- | List of known patterns and their variants
-- | Whether to prefer first or last match
-- | Input words to search
-- | Found pattern if any
findPattern :: PatternList -> MatchPreference -> [Text] -> Maybe PatternMatch
findPattern patterns pref words =
    let matches =
            [ (basePattern, before, drop (length variant) after)
            | i <- [0..length words - 1]
            , let (before, after) = splitAt i words
            , not (null after)
            , (basePattern, variants) <- patterns
            , variant <- variants
            , length variant <= length after
            , take (length variant) after == variant
            ]
    in case pref of
        First -> listToMaybe matches
        Last  -> listToMaybe (reverse matches)


knownPreps :: PatternList
knownPreps =
    [ ("in",    [["in"], ["into"], ["inside"], ["within"]])
    , ("on",    [["on"], ["onto"], ["upon"], ["on", "top", "of"]])
    , ("at",    [["at"]])
    , ("to",    [["to"], ["toward"], ["towards"]])
    , ("from",  [["from"]])
    , ("under", [["under"], ["underneath"], ["beneath"]])
    ]

knownCondNegations :: [Text]
knownCondNegations = ["no", "n't", "not"]

knownCondPatterns :: PatternList
knownCondPatterns =
     [ ("at", [["is", "in"], ["is", "not", "in"], ["is", "inside"], ["is", "not", "inside"], ["inside"], ["not", "inside"], ["isn't", "in"], ["aren't", "in"], ["aren't"], ["at"], ["is", "at"], ["is", "not", "at"]])
     , ("is",  [["is"], ["is", "not"], ["not"], ["are"], ["are", "not"]])
     , ("has", [["has"], ["has", "no"], ["does", "not", "have"], ["doesn't", "have"], ["don't", "have"], ["owns"], ["carries"], ["possesses"], ["holds"], ["doesn't", "hold"], ["doesn't", "posses"], ["doesn't", "carry"]])
    ]

hasClause :: Text -> [Text] -> Bool
hasClause p clauses =
    let lowP = toLower p
    in any (`isInfixOf` lowP) clauses

isCondTypeOf :: Text -> ConditionalType
isCondTypeOf t = case findMatchingPattern t of
    Just ("is", True)   -> NegState
    Just ("is", False)  -> PosState
    Just ("has", True)  -> NonPossessive
    Just ("has", False) -> Possessive
    Just ("at", True)   -> NotAtLocation
    Just ("at", False)  -> AtLocation
    Just {}             -> UnknownConditionalType
    Nothing             -> UnknownConditionalType

-- Todo: Refactor so that instead of `hasNeg` it gives the ConditionalType instead as part of the tuple.
findMatchingPattern :: Text -> Maybe (Text, Bool)
findMatchingPattern input =
    let lowered = toLower input
        matchPattern (patternName, variants) =
            let matches = any (hasClause lowered . pure . Data.Text.unwords) variants
                hasNeg = any (hasClause lowered . pure . Data.Text.unwords)
                         (filter (any (`elem` knownCondNegations)) variants)
            in if matches
               then Just (patternName, hasNeg)
               else Nothing
    in listToMaybe $ mapMaybe matchPattern knownCondPatterns

knownArticles :: [Text]
knownArticles = ["the", "a", "an", "any"]

verbsRequiringObjects :: [Text]
verbsRequiringObjects = ["put", "place", "move", "set"]
