module Core.Message.Common (CommonMessage (..), MessageRenderer (..)) where

import           Data.Text (Text)

-- Base type class for all message renderers
class MessageRenderer a where
    renderMessage :: a -> Text

-- Common messages that might be used across different commands
data CommonMessage
    = ItemDoesNotExist Text
    | LocationDoesNotExist Text
    | NoLocationSpecified
    | PENDING
    deriving (Eq, Show)

instance MessageRenderer CommonMessage where
    renderMessage = \case
        ItemDoesNotExist item -> "Item does not exist in this game world: " <> item <> "."
        LocationDoesNotExist loc -> "Location does not exist in this game world: " <> loc <> "."
        NoLocationSpecified -> "Unable to find a location at all."
        PENDING -> "Pending, not sure what to do with this yet."
