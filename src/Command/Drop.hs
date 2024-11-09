module Command.Drop (module Command.Drop) where
import           Command.Messages
import           Control.Monad.State
import           Core.State
import           Data.Text
import           Parser.Types

import           Utils

dropObject :: Text -> Maybe Text -> Location -> GameWorld -> State GameWorld Text
dropObject object dstM actorLoc gw =
    case findItemByTag object gw of
        Just item -> do
            let updatedGW = moveItemLoc item actorLoc gw
                inv = oxfordEntityNames (getActorInventoryItems updatedGW)
            put updatedGW
            case dstM of
                Nothing -> return $ renderMessage $ DroppedItemWithInventory object inv
                Just dst -> return $ renderMessage $ DroppedItemSomewhere object dst
        Nothing -> return $ renderMessage $ YouDoNotHave object

executeDrop :: CommandExecutor
executeDrop expr = do
    gw <- get
    let acLoc = getActiveActorLoc gw
        handle = \case
            AtomicExpression _ ->
                return $ renderMessage DropWhat
            UnaryExpression _ (NounClause object) ->
                dropObject object Nothing acLoc gw
            BinaryExpression {} ->
                return $ renderMessage DropWhat
            ComplexExpression _ (NounClause object) (PrepClause prep) (NounClause dst) ->
                dropObject object (Just (prep <> " " <> dst)) acLoc gw
    handle expr
