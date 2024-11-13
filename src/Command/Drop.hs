module Command.Drop (module Command.Drop) where
import           Command.CommandExecutor
import           Core.GameMonad
import           Core.Message
import           Core.State
import           Data.Text
import           Parser.Types

import           Utils

dropObject :: Text -> Maybe Text -> Text -> World -> GameStateText
dropObject object dstM actorLoc gw = undefined
    -- case findEntityById (EntityId object) gw of
    --     Just item -> do
    --         let updatedGW = moveItemLoc item actorLoc gw
    --             inv = oxfordEntityNames (getActorInventoryItems updatedGW)
    --         modifyWorld (const updatedGW)
    --         case dstM of
    --             Nothing  -> msg $ DroppedItemWithInventory object inv
    --             Just dst -> msg $ DroppedItemSomewhere object dst
    --     Nothing -> msg $ YouDoNotHave object

executeDrop :: CommandExecutor
executeDrop expr = do
    gw <- getWorld
    let handle = \case
            AtomicExpression _ ->
                msg DropWhat
            UnaryExpression _ (NounClause object) ->
                dropObject object Nothing "TBD LOCATION" gw
            BinaryExpression {} ->
                msg DropWhat
            ComplexExpression _ (NounClause object) (PrepClause prep) (NounClause dst) -> dropObject object (Just (prep <> " " <> dst)) "TBD LOCATION" gw
    handle expr
