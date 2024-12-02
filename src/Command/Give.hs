module Command.Give (module Command.Give) where

import           Command.CommandExecutor
import           Command.Message
import           Core.GameMonad
import           Core.State
import           Data.Text               (Text)
import           Entity.Class.Capacity   (changeItemContainer)
import           Entity.Class.EntityBase (getName, getId)
import           Entity.Entity
import           Entity.Types.Common     (EntityId(..))
import           Parser.Types
import           Parser.Utils            (isPrepVariantOf)
import           Utils                   (ItemTag)

giveItemToActor :: ItemTag -> Text -> World -> GameStateText
giveItemToActor itemTag targetActorTag gw =
    case findEntityById itemId gw of
        Just (ItemResult item) | item `elem` itemsOnActor ->
            case findEntityById targetActorId gw of
                Just (ActorResult target) | target `elem` actorsInLoc ->
                    transferItem item target
                _ -> msg $ NoActorForItem itemTag targetActorTag
        _ -> msg $ YouDoNotHave itemTag
    where
        itemId = EntityId itemTag
        targetActorId = EntityId targetActorTag
        itemsOnActor = getActiveActorInventoryList gw

        currentLocationId = getId (getActiveActorLocation gw)
        actorsInLoc = getActorsAtLocation currentLocationId gw

        transferItem :: Entity 'ItemT -> Entity 'ActorT -> GameStateText
        transferItem item target =
            case changeItemContainer target item gw of
                Right updatedGW -> do
                    modifyWorld (const updatedGW)
                    msg $ GaveItem itemTag (getName target)
                Left errMsg -> return errMsg

executeGive :: CommandExecutor
executeGive expr = do
    gw <- getWorld
    case expr of
        AtomicCmdExpression {} ->
            msg GiveWhat
        UnaryCmdExpression _ (NounClause itemTag) ->
            msg $ GiveToWhom itemTag
        BinaryCmdExpression {} ->
            msg GiveWhat
        ComplexCmdExpression _ (NounClause itemTag) (PrepClause prep) (NounClause actorTag)
            | prep `isPrepVariantOf` "to" ->
                giveItemToActor itemTag actorTag gw
        ComplexCmdExpression {} ->
            msg NotSure