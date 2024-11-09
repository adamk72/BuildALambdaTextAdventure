module Command.Put (module Command.Put) where

import           Command.CommandExecutor
import           Control.Monad.State
import           Core.Message
import           Core.State
import           Data.Text               (Text)
import           Parser.Types

putItemInContainer :: Text -> Text -> [Text] -> GameWorld -> State GameWorld Text
putItemInContainer itemTag containerTag validItemTags gw
    | itemTag `elem` validItemTags && containerTag `elem` validItemTags =
        case findItemByTag itemTag gw of
            Nothing ->  msgGameWordError $ ItemError itemTag
            Just foundItem -> case findItemByTag containerTag gw of
                Nothing -> msgGameWordError $ ItemError containerTag
                Just validContainer -> case getInventory validContainer of
                    Nothing -> msg $ NotAContainer containerTag
                    Just containerLoc -> do
                        let updatedGW = moveItemLoc foundItem containerLoc gw
                        put updatedGW
                        msg $ PutItemIn itemTag containerTag
    | itemTag `elem` validItemTags && notElem containerTag validItemTags = msg $    NoContainerForItem itemTag containerTag
    | notElem  itemTag validItemTags && containerTag `elem` validItemTags = msg $ NoItemForContainer itemTag containerTag
    | otherwise = msg $ InvalidItemInLocation itemTag

executePut :: CommandExecutor
executePut expr = do
    gw <- get
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
