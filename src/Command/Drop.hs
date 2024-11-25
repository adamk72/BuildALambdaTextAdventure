module Command.Drop (module Command.Drop) where
import           Command.CommandExecutor
import           Command.Message
import           Core.GameMonad
import           Core.State
import           Entity.Class.Capacity
import           Entity.Entity
import           Parser.Types
import           Utils                   (ContainerTag, ItemTag)

dropObject :: ItemTag -> Maybe ContainerTag -> World -> GameStateText
dropObject itemTag dstTagM gw =
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

executeDrop :: CommandExecutor
executeDrop expr = do
    gw <- getWorld
    let handle = \case
            AtomicExpression _ ->
                msg DropWhat
            UnaryExpression _ (NounClause object) ->
                dropObject object Nothing  gw
            BinaryExpression {} ->
                msg DropWhat
            ComplexExpression _ (NounClause object) (PrepClause prep) (NounClause dst) -> dropObject object (Just (prep <> " " <> dst)) gw
    handle expr
