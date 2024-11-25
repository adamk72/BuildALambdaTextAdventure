module Command.Message.Inventory (InventoryMessage (..), showInventoryList) where

import           Command.Message.Common  (MessageRenderer (..))
import           Data.Text               (Text)
import           Entity.Class.Capacity
import           Entity.Class.EntityBase
import           Entity.Ops.NestedInventory
import           Entity.Entity
import           Utils                   (oxfordEntityNames)

data InventoryMessage
    = PickedUp Text Text
    | DroppedItem Text
    | DroppedItemWithInventory Text Text
    | YouDoNotHave Text
    | LookAround Text
    | AlreadyHaveItem Text
    | NotFoundIn Text Text         -- item, container/location
    | NotAContainer Text           -- container name
    deriving (Eq, Show)

instance MessageRenderer InventoryMessage where
    renderMessage = \case
        PickedUp item actor -> "Moved " <> item <> " to " <> actor
        DroppedItem object -> "You dropped " <> object <> "."
        DroppedItemWithInventory object invText ->
            "You dropped " <> object <> ". " <> invText
        YouDoNotHave object -> "You don't have a " <> object <> " to drop."
        LookAround objs -> "You look around and see " <> objs <> "."
        AlreadyHaveItem item -> "You already have " <> item
        NotFoundIn item container -> "Cannot find " <> item <> " in " <> container
        NotAContainer container -> container <> " isn't a container."

showInventoryList :: World -> Text
showInventoryList w = do
    let nestedItems = getNestedInventoryList (activeActor w) w
    if null nestedItems
        then "Your inventory is empty."
        else "Your inventory contains: " <> formatNestedInventory nestedItems <> "."
