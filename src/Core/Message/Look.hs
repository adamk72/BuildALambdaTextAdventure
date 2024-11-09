module Core.Message.Look
    ( LookMessage(..)
    ) where

import Data.Text (Text, toLower)
import Core.Message.Common (MessageRenderer(..))

data LookMessage
    = LookTowards Text
    deriving (Eq, Show)

instance MessageRenderer LookMessage where
    renderMessage = \case
        LookTowards dir -> "You look " <> toLower dir <> ", but see nothing special."