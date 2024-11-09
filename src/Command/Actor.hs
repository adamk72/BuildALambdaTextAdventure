module Command.Actor (module Command.Actor) where
import           Command.Messages
import           Core.State.Operations
import           Control.Monad.State
import           Utils

executeInventory :: CommandExecutor
executeInventory _ = do
  gw <- get
  return $ "Your inventory is: " <> oxfordEntityNames (getActorInventoryItems gw)