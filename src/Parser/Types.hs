module Parser.Types (Expression (..), NounClause (..), ParseError (..), PrepClause (..)) where

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

data Expression =
      AtomicExpression Verb
    | UnaryExpression Verb NounClause
    | BinaryExpression Verb PrepClause NounClause
    | ComplexExpression Verb NounClause PrepClause NounClause
    deriving (Show, Eq)

data ParseError =
      MissingObject
    | MissingTarget
    | MalformedExpression Text
    deriving (Show, Eq)
