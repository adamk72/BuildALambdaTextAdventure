module Command.Drop (module Command.Drop) where
import           Command.CommandExecutor
import           Core.GameMonad
import           Core.Message
import           Core.State
import           Data.Text
import           Parser.Types

import           Utils

dropObject :: Text -> Maybe Text -> Location -> GameWorld -> GameStateText
dropObject object dstM actorLoc gw =
    case findItemByTag object gw of
        Just item -> do
            let updatedGW = moveItemLoc item actorLoc gw
                inv = oxfordEntityNames (getActorInventoryItems updatedGW)
            modifyGameWorld (const updatedGW)
            case dstM of
                Nothing  -> msg $ DroppedItemWithInventory object inv
                Just dst -> msg $ DroppedItemSomewhere object dst
        Nothing -> msg $ YouDoNotHave object

executeDrop :: CommandExecutor
executeDrop expr = do
    gw <- getGameWorld
    let acLoc = getActiveActorLoc gw
        handle = \case
            AtomicExpression _ ->
                msg DropWhat
            UnaryExpression _ (NounClause object) ->
                dropObject object Nothing acLoc gw
            BinaryExpression {} ->
                msg DropWhat
            ComplexExpression _ (NounClause object) (PrepClause prep) (NounClause dst) -> dropObject object (Just (prep <> " " <> dst)) acLoc gw
    handle expr
