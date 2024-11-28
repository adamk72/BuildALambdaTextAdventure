module Parser.Types (CmdExpression (..), NounClause (..), ParseError (..), PrepClause (..), Subject, CondClause(..), CondExpression(..)) where

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

newtype CondClause = CondClause { unCondClause :: Text} deriving (Show, Eq)

type Subject = Text

data CondExpression =
      UnaryCondExpression Subject CondClause
    deriving (Show, Eq)

data ParseError =
      MissingObject
    | MissingTarget
    | MalformedExpression Text
    | TBDError
    deriving (Show, Eq)
