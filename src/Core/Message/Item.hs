module Core.Message.Item
    ( ItemMessage(..)
    ) where

import Data.Text (Text)
import Core.Message.Common (MessageRenderer(..))

data ItemMessage
    = DoNotSeeItem Text
    | InvalidItem Text
    | NoItem Text
    | InvalidItemInLocation Text
    | InvalidItemInContainer Text Text
    | DroppedItemSomewhere Text Text
    deriving (Eq, Show)

instance MessageRenderer ItemMessage where
    renderMessage = \case
        DoNotSeeItem item -> "Don't see a " <> item <> "."
        DroppedItemSomewhere object loc -> "You dropped " <> object <> " " <> loc <> "."
        InvalidItem item -> "Don't see a \"" <> item <> "\"."
        NoItem item -> "There is no indication there's a \"" <> item <> "\" around here."
        InvalidItemInLocation item -> "Don't see a \"" <> item <> "\" around here."
        InvalidItemInContainer item container ->
            "Don't see a \"" <> item <> "\" to put into " <> container <> "\"."
