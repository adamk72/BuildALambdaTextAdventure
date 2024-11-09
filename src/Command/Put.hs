module Command.Put (module Command.Put) where

import           Command.Messages
import           Control.Monad.State
import           Core.State
import           Data.Text (Text)
import           Parser.Types

putItemInContainer :: Text -> Text -> [Text] -> GameWorld -> State GameWorld Text
putItemInContainer itemTag containerTag validItemTags gw
    | itemTag `elem` validItemTags =
        case findItemByTag itemTag gw of
            Nothing -> msg $ NoPath itemTag
            Just item -> case findItemByTag containerTag gw of
                Nothing -> msg $ InvalidItem itemTag
                Just container -> case getInventory container of
                    Nothing -> msg $ NotAContainer containerTag
                    Just containerLoc -> do
                        let updatedGW = moveItemLoc item containerLoc gw
                        put updatedGW
                        msg $ PutItemIn itemTag containerTag
    | otherwise = msg $ InvalidItem itemTag

executePut :: CommandExecutor
executePut expr = do
    gw <- get
    let acLoc = getActiveActorLoc gw
        validItemTags = map getTag $ getItemsAtLoc acLoc gw
    case expr of
        AtomicExpression {} ->
            msg PutWhat
        UnaryExpression _ (NounClause itemTag) ->
            msg $ PutWhere itemTag
        BinaryExpression {} ->
            msg PutWhat
        ComplexExpression _ (NounClause itemTag) _ (NounClause containerTag) ->
           putItemInContainer itemTag containerTag validItemTags gw
