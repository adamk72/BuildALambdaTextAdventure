module Command.Actor (module Command.Actor) where
import           Command.CommandExecutor
import           Control.Monad.State
import           Core.State.Operations
import           Utils

executeInventory :: CommandExecutor
executeInventory _ = do
  gw <- get
  return $ "Your inventory is: " <> oxfordEntityNames (getActorInventoryItems gw)
