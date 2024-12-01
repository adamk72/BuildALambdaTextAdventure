module Parser.Types
    ( CmdExpression (..)
    , CondExpression (..)
    , ConditionalType (..)
    , NounClause (..)
    , ParseError (..)
    , PossessionClause (..)
    , PrepClause (..)
    , StateClause (..)
    , SubjClause (..)
    , Subject
    ) where

import           Data.Text

{-
- `phrase`              => any arbitrary sentence.
- `expression` (`exp`)  => the result of parsing a phrase which is well structured.
- `command` (`cmd`)     => phrase that starts with a known good verb (e.g., "go", "look").
- `clause`              => the portion of a phrase after the verb. There are two types:
  - 'noun clause'
  - 'prepositional clause'
-}

newtype NounClause = NounClause { unNounClause :: Text }
    deriving (Show, Eq)

newtype PrepClause = PrepClause { unPrepClause :: Text }
    deriving (Show, Eq)

type Verb = Text

data CmdExpression =
      AtomicCmdExpression Verb
    | UnaryCmdExpression Verb NounClause
    | BinaryCmdExpression Verb PrepClause NounClause
    | ComplexCmdExpression Verb NounClause PrepClause NounClause
    deriving (Show, Eq)

newtype SubjClause = SubjClause { unSubjClause :: Text} deriving (Show, Eq)
newtype StateClause = StateClause { unStateClause :: Text} deriving (Show, Eq)
newtype PossessionClause = PossessionClause { unPossessionClause :: Text} deriving (Show, Eq)

type Subject = Text

data CondExpression =
      PosStateExpression SubjClause StateClause
    | NegStateExpression SubjClause StateClause
    | PossessiveExpression SubjClause PossessionClause
    | NonPossessiveExpression SubjClause PossessionClause
    | AtLocationExpression SubjClause StateClause
    | NotAtLocationExpression SubjClause StateClause
    deriving (Show, Eq)

data ConditionalType = PosState | NegState | Possessive | NonPossessive | UnknownConditionalType | AtLocation | NotAtLocation
    deriving (Show, Eq)

data ParseError =
      MissingObject
    | MissingTarget
    | MalformedCmdExpression Text
    | MalformedCondExpression Text
    | TBDError
    deriving (Show, Eq)
