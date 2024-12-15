module Command.Message.Give (GiveMessage (..)) where

import           Command.Message.Common (MessageRenderer (..))
import           Data.Text              (Text)

data GiveMessage
  = GiveWhat
  | GiveToWhom Text
  | NoActorForItem Text Text
  | GaveItem Text Text
  deriving (Eq, Show)

instance MessageRenderer GiveMessage where
  renderMessage = \case
    GiveWhat -> "What do you want to give?"
    GiveToWhom item -> "Who do you want to give the " <> item <> " to?"
    NoActorForItem item actor ->
      "Don't see " <> actor <> " here to give the " <> item <> " to."
    GaveItem item actor -> "You gave the " <> item <> " to " <> actor <> "."
