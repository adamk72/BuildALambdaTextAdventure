module Command.Message.Look (LookMessage (..)) where

import           Command.Message.Common (MessageRenderer (..))
import           Data.Text              (Text)
import qualified Data.Text              as T

data LookMessage
    = YouSeeGeneral Text
    | LookIn Text Text
    deriving (Eq, Show)

instance MessageRenderer LookMessage where
    renderMessage = \case
        YouSeeGeneral msg -> msg
        LookIn container contents ->
            if T.null contents
            then "The " <> container <> " is empty."
            else "Inside the " <> container <> " you see " <> contents <> "."
