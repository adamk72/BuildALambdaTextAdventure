module Command.Drop (module Command.Drop) where
import           Command.Common
import           Control.Monad.State
import Core.State
import Utils
import Data.Maybe (fromJust)

executeDrop :: CommandExecutor
executeDrop target = do
  gw <- get
  let ac = gwActiveActor gw
      acLoc = gwActiveActorLoc gw
      validLocItems = getItemsAtLoc acLoc gw
      -- validActorItems = gwActiveActorLoc
  return $ "Items at location: " <> fromJust target <> " with " <> oxfordEntityNames validLocItems