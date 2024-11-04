module Command.Drop (module Command.Drop) where
import           Command.Common
import           Control.Monad.State
import Core.State
import Utils
import Data.Maybe (fromJust)

executeDrop :: CommandExecutor
executeDrop target = do
  gw <- get
  let ac = gwActiveCharacter gw
      acLoc = getActiveCharLoc gw
      validLocItems = getItemsAtLoc acLoc gw
      -- validActorItems = getActiveCharLoc
  return $ "Items at location: " <> fromJust target <> " with " <> oxfordEntityNames validLocItems