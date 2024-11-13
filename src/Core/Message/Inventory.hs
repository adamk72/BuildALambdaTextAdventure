module Core.Message.Inventory (InventoryMessage (..)) where

import           Core.Message.Common (MessageRenderer (..))
import           Data.Text           (Text)
import           Utils               (oxfordEntityNames)
import Entity.Entity

data InventoryMessage
    = PickedUp Text Text
    | DroppedItem Text
    | DroppedItemWithInventory Text Text
    | YouDoNotHave Text
    | LookAround [Entity 'ItemT]
    | AlreadyHaveItem Text
    | NotFoundIn Text Text         -- item, container/location
    | NotAContainer Text           -- container name
    deriving (Eq, Show)

instance MessageRenderer InventoryMessage where
    renderMessage = \case
        PickedUp item actor -> "Moved " <> item <> " to " <> actor
        DroppedItem object -> "You dropped " <> object <> "."
        DroppedItemWithInventory object inv ->
            "You dropped " <> object <> ". Your inventory is now: " <> inv <> "."
        YouDoNotHave object -> "You don't have a " <> object <> " to drop."
        LookAround objs -> "You look around and see " <> oxfordEntityNames objs <> "."
        AlreadyHaveItem item -> "You already have " <> item
        NotFoundIn item container -> "Cannot find " <> item <> " in " <> container
        NotAContainer container -> container <> " isn't a container."