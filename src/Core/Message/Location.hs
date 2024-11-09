module Core.Message.Location
    ( LocationMessage(..)
    ) where

import Data.Text (Text, toLower)
import Core.Message.Common (MessageRenderer(..))

data LocationMessage
    = AlreadyAtLocation Text
    | MovingToLocation Text
    | NoPath Text
    | YouAreIn Text
    deriving (Eq, Show)

instance MessageRenderer LocationMessage where
    renderMessage = \case
        AlreadyAtLocation loc -> "You're already in " <> loc <> "."
        MovingToLocation loc -> "Moving to " <> loc <> "."
        NoPath loc -> "There is no indication there's a way to get to \"" <> loc <> "\"."
        YouAreIn loc ->  "You are in " <> toLower loc <> "."