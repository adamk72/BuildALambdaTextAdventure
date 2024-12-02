module Parser.Patterns
    ( CondPatternList
    , CondPatternMatch
    , MatchPreference (..)
    , PatternType (..)
    , PrepPatternList
    , PrepPatternMatch
    , findPattern
    , knownArticles
    , knownCondPatterns
    , knownPreps
    , verbsRequiringObjects
    ) where

import           Data.Maybe (listToMaybe)
import           Data.Text  (Text)
import           Prelude    hiding (words)

type CondPatternList = [(PatternType, [[Text]])]
type PrepPatternList = [(Text, [[Text]])]

type CondPatternMatch = (PatternType, [Text], [Text])
type PrepPatternMatch = (Text, [Text], [Text])

data PatternType = PosState | NegState | Possessive | NonPossessive | AtLocation | NotAtLocation
    deriving (Show, Eq)

data MatchPreference = First | Last
    deriving (Eq, Show)

-- Todo: Fix. This is _not_ greedy/exact, so the PatternList needs to be properly ordered to ensure it behaves as expected with multi-words.
-- For example, when working with "is not", if "is" is found first, then "is not" will be skipped.
findPattern :: [(a, [[Text]])] -> MatchPreference -> [Text] -> Maybe (a, [Text], [Text])
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

knownPreps :: PrepPatternList
knownPreps =
    [ ("in",    [["in"], ["into"], ["inside"], ["within"]])
    , ("on",    [["on"], ["onto"], ["upon"], ["on", "top", "of"]])
    , ("at",    [["at"]])
    , ("to",    [["to"], ["toward"], ["towards"]])
    , ("from",  [["from"]])
    , ("under", [["under"], ["underneath"], ["beneath"]])
    ]

-- Order is important here; see note with `findPattern`.
knownCondPatterns :: CondPatternList
knownCondPatterns =
     [ (NotAtLocation, [["is", "not", "in"], ["is", "not", "inside"], ["not", "inside"], ["isn't", "in"], ["are", "not", "in"], ["aren't", "in"], ["is", "not", "at"], ["not", "at"], ["isn't", "at"]])
     , (AtLocation, [["is", "in"], ["is", "inside"], ["inside"], ["at"], ["is", "at"]])
     , (NegState, [["is", "not"], ["not"], ["aren't"], ["are", "not"]])
     , (PosState, [["is"], ["are"]])
     , (NonPossessive, [["has", "no"], ["does", "not", "have"], ["doesn't", "have"], ["don't", "have"], ["doesn't", "hold"], ["doesn't", "posses"], ["doesn't", "carry"], ["does", "not", "hold"], ["does", "not", "posses"], ["does", "not", "carry"]])
     , (Possessive, [["has"], ["owns"], ["carries"], ["possesses"], ["holds"]])
    ]

knownArticles :: [Text]
knownArticles = ["the", "a", "an"]

verbsRequiringObjects :: [Text]
verbsRequiringObjects = ["put", "place", "move", "set"]
