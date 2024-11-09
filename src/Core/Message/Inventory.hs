module Core.Message.Inventory (InventoryMessage (..)) where

import           Core.Message.Common (MessageRenderer (..))
import           Core.State          (Item)
import           Data.Text           (Text)
import           Utils               (oxfordEntityNames)

data InventoryMessage
    = PickedUp Text Text
    | DroppedItem Text
    | DroppedItemWithInventory Text Text
    | YouDoNotHave Text
    | LookAround [Item]
    deriving (Eq, Show)

instance MessageRenderer InventoryMessage where
    renderMessage = \case
        PickedUp item actor -> "Moved " <> item <> " to " <> actor
        DroppedItem object -> "You dropped " <> object <> "."
        DroppedItemWithInventory object inv ->
            "You dropped " <> object <> ". Your inventory is now: " <> inv <> "."
        YouDoNotHave object -> "You don't have a " <> object <> " to drop."
        LookAround objs -> "You look around and see " <> oxfordEntityNames objs <> "."
