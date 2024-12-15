module Command.Drop (module Command.Drop) where

import           Command.Executor
import           Command.Message
import           Core.GameMonad
import           Core.State
import           Entity.Class.Capacity
import           Entity.Entity
import           Entity.Types.Common
import           Parser.Types
import           Utils                 (ItemTag)

dropObject :: ItemTag -> World -> GameStateText
dropObject itemTag gw =
  case findEntityById itemId gw of
    Just (ItemResult item) | item `elem` itemsOnActor -> dropItemAtLocation item
    _                                                 -> msg $ YouDoNotHave itemTag
  where
    itemId = EntityId itemTag
    itemsOnActor = getActiveActorInventoryList gw

    dropItemAtLocation :: Entity 'ItemT -> GameStateText
    dropItemAtLocation item = do
      case changeItemContainer (getActiveActorLocation gw) item gw of
        Right updatedGW -> do
          modifyWorld (const updatedGW)
          msg $ DroppedItemWithInventory itemTag (showInventoryList updatedGW)
        Left errMsg -> return errMsg

executeDrop :: BasicCommandExecutor
executeDrop expr = do
  gw <- getWorld
  let handle = \case
        AtomicCmdExpression _ ->
          msg DropWhat
        UnaryCmdExpression _ (NounClause object) ->
          dropObject object gw
        SplitCmdExpression {} ->
          msg DropWhat
        ComplexCmdExpression _ (NounClause object) _ _ -> dropObject object gw
  handle expr
