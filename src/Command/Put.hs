module Command.Put (module Command.Put) where

import           Command.CommandExecutor
import           Core.GameMonad
import           Core.Message
import           Core.State
import           Data.Text               (Text)
import           Parser.Types

putItemInContainer :: Text -> Text -> [Text] -> World -> GameStateText
putItemInContainer itemTag containerTag validItemTags gw =
    case (findItemByTag itemTag gw, findItemByTag containerTag gw) of
        (Just item, Just container)
            | itemTag `elem` validItemTags && containerTag `elem` validItemTags ->
                if isContainer container
                    then do
                        let updatedGW = moveItemToContainer item container gw
                        case updatedGW of
                            Right newGW -> do
                                modifyWorld (const newGW)
                                msg $ PutItemIn itemTag containerTag
                            Left err -> return err
                    else msg $ NotAContainer containerTag
            | itemTag `elem` validItemTags ->
                msg $ NoContainerForItem itemTag containerTag
            | containerTag `elem` validItemTags ->
                msg $ NoItemForContainer itemTag containerTag
            | otherwise ->
                msg $ InvalidItemInLocation itemTag
        (Nothing, _) -> msgGameWordError $ ItemError itemTag
        (_, Nothing) -> msgGameWordError $ ItemError containerTag

executePut :: CommandExecutor
executePut expr = do
    gw <- getWorld
    let acLoc = getActiveActorLoc gw
        validItemTags = map getTag $ getItemsAtLoc acLoc gw ++ getActorInventoryItems gw
    case expr of
        AtomicExpression {} ->
            msg PutWhat
        UnaryExpression _ (NounClause itemTag) ->
            msg $ PutWhere itemTag
        BinaryExpression {} ->
            msg PutWhat
        ComplexExpression _ (NounClause itemTag) _ (NounClause containerTag) ->
            putItemInContainer itemTag containerTag validItemTags gw
