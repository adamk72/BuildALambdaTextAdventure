module Command.Drop (module Command.Drop) where
import           Command.Messages
import           Core.State.Operations
import           Control.Monad.State
import           Parser.Types
import           Utils

executeDrop :: CommandExecutor
executeDrop expr = do
  gw <- get
  let acLoc = getActiveActorLoc gw
  case expr of
    (UnaryExpression _ (NounClause target)) -> do
      case findItemByTag target gw of
        Just item -> do
          let updatedGW = moveItemLoc item acLoc gw
          put updatedGW
          return $ target <> " dropped. " <> "Your inventory is now: " <> oxfordEntityNames (getActorInventoryItems updatedGW)
        Nothing -> return $ "You don't have a " <> target <> " to drop."
    _ -> return $ renderMessage PENDING