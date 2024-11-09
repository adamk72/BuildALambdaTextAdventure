module Command.Actor (module Command.Actor) where
import           Command.CommandExecutor
import           Control.Monad.State
import           Core.State.Operations
import           Utils

-- Todo: This can call the Look command with a Binary Expression instead ("look inventory").
executeInventory :: CommandExecutor
executeInventory _ = do
  gw <- get
  return $ "Your inventory is: " <> oxfordEntityNames (getActorInventoryItems gw)
