module Command.Message.Location (LocationMessage (..)) where

import           Command.Message.Common (MessageRenderer (..))
import           Data.Text              (Text, toLower)

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
    YouAreIn loc -> "You are in " <> toLower loc <> "."
