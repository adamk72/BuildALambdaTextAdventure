
module Parser.Internal.Patterns
    ( MatchPreference (..)
    , PatternList
    , PatternMatch
    , findPattern
    , knownArticles
    , knownPatterns
    , knownPreps
    , verbsRequiringObjects
    ) where

import           Data.Maybe (listToMaybe)
import           Data.Text  (Text)
import           Prelude    hiding (words)

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

knownPatterns :: PatternList
knownPatterns =
    [ ("is",  [["is"], ["are"], ["was"], ["were"]])
    , ("has", [["has"], ["have"], ["had"]])
    , ("not", [["not"], ["doesn't"], ["does", "not"], ["has", "no"], ["don't"]])
    ]

knownArticles :: [Text]
knownArticles = ["the", "a", "an"]

verbsRequiringObjects :: [Text]
verbsRequiringObjects = ["put", "place", "move", "set"]
