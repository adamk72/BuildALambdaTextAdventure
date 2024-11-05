module Command.Drop (module Command.Drop) where
import           Command.Common
import           Core.State.Operations
import           Control.Monad.State
import           Core.State
import           Data.Maybe          (fromJust)
import           Utils

executeDrop :: CommandExecutor
executeDrop target = do
  gw <- get
  let ac = gwActiveActor gw
      acLoc = getActiveActorLoc gw
      validLocItems = getItemsAtLoc acLoc gw
      pocketItems = getActorInventory gw
  return $ "Items at location: " <> fromJust target <> " with " <> oxfordEntityNames pocketItems
