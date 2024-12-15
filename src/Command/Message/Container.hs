module Command.Message.Container (ContainerMessage (..)) where

import           Command.Message.Common (MessageRenderer (..))
import           Data.Text              (Text)

data ContainerMessage
    = PutItemIn Text Text
    | NoItemForContainer Text Text
    | NoContainerForItem Text Text
    deriving (Eq, Show)

instance MessageRenderer ContainerMessage where
    renderMessage = \case
        PutItemIn item dst -> item <> " is now in the " <> dst <> "."
        NoItemForContainer item container ->
            "Don't see " <> item <> " to put in " <> container <> "."
        NoContainerForItem item container ->
            "Don't have " <> container <> " to put " <> item <> "."
