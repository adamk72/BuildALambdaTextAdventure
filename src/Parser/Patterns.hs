module Parser.Patterns
    ( CondPatternList
    , CondPatternMatch
    , CondPatternType (..)
    , MatchPreference (..)
    , PrepPatternList
    , PrepPatternMatch
    , findPattern
    , knownArticles
    , knownCondPatterns
    , knownPluralEntityClasses
    , knownPreps
    , verbsRequiringObjects
    ) where

import           Data.Maybe (listToMaybe)
import           Data.Text  (Text)
import           Prelude    hiding (words)

type CondPatternList = [(CondPatternType, [[Text]])]

type PrepPatternList = [(Text, [[Text]])]

type CondPatternMatch = (CondPatternType, [Text], [Text])

type PrepPatternMatch = (Text, [Text], [Text])

data CondPatternType = PosState | NegState | Possessive | NonPossessive | AtLocation | NotAtLocation
  deriving (Show, Eq)

data MatchPreference = First | Last
  deriving (Eq, Show)

-- Todo: Fix. This is _not_ greedy/exact, so the PatternList needs to be properly ordered to ensure it behaves as expected with multi-words.
-- For example, when working with "is not", if "is" is found first, then "is not" will be skipped.
findPattern :: [(a, [[Text]])] -> MatchPreference -> [Text] -> Maybe (a, [Text], [Text])
findPattern patterns pref words =
  let matches =
        [ (basePattern, before, drop (length variant) after)
          | i <- [0 .. length words - 1],
            let (before, after) = splitAt i words,
            not (null after),
            (basePattern, variants) <- patterns,
            variant <- variants,
            length variant <= length after,
            take (length variant) after == variant
        ]
   in case pref of
        First -> listToMaybe matches
        Last  -> listToMaybe (reverse matches)

knownPreps :: PrepPatternList
knownPreps =
  [ ("in", [["in"], ["into"], ["inside"], ["within"]]),
    ("on", [["on"], ["onto"], ["upon"], ["on", "top", "of"]]),
    ("at", [["at"]]),
    ("to", [["to"], ["toward"], ["towards"]]),
    ("from", [["from"]]),
    ("under", [["under"], ["underneath"], ["beneath"]]),
    ("with", [["with"]])
  ]

-- Order is important here; see note with `findPattern`.
knownCondPatterns :: CondPatternList
knownCondPatterns =
  [ ( NotAtLocation,
      [ ["are", "not", "in"],
        ["aren't", "in"],
        ["is", "not", "at"],
        ["is", "not", "in"],
        ["is", "not", "inside"],
        ["isn't", "at"],
        ["isn't", "in"],
        ["not", "at"],
        ["not", "inside"]
      ]
    ),
    ( AtLocation,
      [ ["inside"],
        ["is", "at"],
        ["is", "in"],
        ["is", "inside"],
        ["at"]
      ]
    ),
    ( NonPossessive,
      [ ["is", "not", "carrying"],
        ["is", "not", "holding"],
        ["is", "not", "possessing"],
        ["does", "not", "carry"],
        ["does", "not", "have"],
        ["does", "not", "hold"],
        ["does", "not", "own"],
        ["does", "not", "posses"],
        ["doesn't", "carry"],
        ["doesn't", "have"],
        ["doesn't", "hold"],
        ["doesn't", "own"],
        ["doesn't", "posses"],
        ["has", "no"]
      ]
    ),
    ( Possessive,
      [ ["has"],
        ["owns"],
        ["carries"],
        ["possesses"],
        ["holds"],
        ["does", "have"]
      ]
    ),
    ( NegState,
      [ ["is", "not"],
        ["is", "not", "of", "type"],
        ["is", "not", "type", "of"],
        ["not"],
        ["not", "of", "type"],
        ["not", "type", "of"],
        ["aren't"],
        ["are", "not"],
        ["no", "type"],
        ["has", "no", "type"],
        ["has", "no", "type", "of"],
        ["type", "is", "not"],
        ["is", "not", "type"],
        ["is", "not", "of", "type"],
        ["who", "does", "not", "have", "type"],
        ["who", "does", "not", "have", "type", "of"]
      ]
    ),
    ( PosState,
      [ ["who", "is"],
        ["who", "is", "type", "of"],
        ["who", "have"],
        ["who", "have", "type", "of"],
        ["who", "have", "type", "of"],
        ["who", "have", "of", "type"],
        ["who", "has"],
        ["who", "has", "type", "of"],
        ["who", "are"],
        ["who", "are", "type", "of"],
        ["who", "are", "type", "of"],
        ["who", "are", "of", "type"],
        ["which", "is"],
        ["which", "is", "type", "of"],
        ["which", "have"],
        ["which", "have", "type", "of"],
        ["which", "have", "type", "of"],
        ["which", "have", "of", "type"],
        ["which", "has"],
        ["which", "has", "type", "of"],
        ["which", "are"],
        ["which", "are", "type", "of"],
        ["which", "are", "type", "of"],
        ["which", "are", "of", "type"],
        ["type", "is"],
        ["is", "type"],
        ["is", "type", "of"],
        ["is", "of", "type"],
        ["has", "type"],
        ["are"],
        ["are", "type", "of"],
        ["are", "of", "type"],
        ["type"],
        ["with"],
        ["is"],
        ["is"]
      ]
    )
  ]

knownPluralEntityClasses :: [Text]
knownPluralEntityClasses = ["actors", "items", "locations"]

knownArticles :: [Text]
knownArticles = ["the", "a", "an"]

verbsRequiringObjects :: [Text]
verbsRequiringObjects = ["put", "place", "move", "set"]
