module Command.Drop (module Command.Drop) where
import           Command.Common
import           Core.State.Operations
import           Control.Monad.State
import           Utils

executeDrop :: CommandExecutor
executeDrop (Just target) = do
  gw <- get
  let acLoc = getActiveActorLoc gw
  case findItemByTag target gw of
    Just item -> do
      let updatedGW = moveItemLoc item acLoc gw
      put updatedGW
      return $ target <> " dropped. " <> "Your inventory is now: " <> oxfordEntityNames (getActorInventory updatedGW)
    Nothing -> return $ "You don't have a " <> target <> " to drop."
executeDrop Nothing = return $ "You don't have anything to drop."
