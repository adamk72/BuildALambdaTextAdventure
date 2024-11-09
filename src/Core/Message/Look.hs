module Core.Message.Look
    ( LookMessage(..)
    ) where

import Data.Text (Text, toLower)
import Core.Message.Common (MessageRenderer(..))

data LookMessage
    = LookTowards Text
    | LookAtWhat
    | YouSeeGeneral Text
    deriving (Eq, Show)

instance MessageRenderer LookMessage where
    renderMessage = \case
        YouSeeGeneral environment -> "You take a quick glance around and see " <> environment <> "."
        LookAtWhat -> "What do you want to look at?"
        LookTowards dir -> "You look " <> toLower dir <> ", but see nothing special."