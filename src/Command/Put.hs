module Command.Put (module Command.Put) where

import           Command.CommandExecutor
import           Core.GameMonad
import           Command.Message
import           Core.State
import           Data.Text               (Text)
import           Parser.Types

putItemInContainer :: Text -> Text -> World -> GameStateText
putItemInContainer itemTag containerTag gw = undefined
    -- case (findEntityByTag itemTag gw, findEntityByTag containerTag gw) of
    --     (Just item, Just container)
    --         | itemTagExistsAtActorLoc itemTag gw && itemTagExistsAtActorLoc containerTag gw ->
    --             if isContainer container
    --                 then do
    --                     let updatedGW = moveItemToContainer item container gw
    --                     case updatedGW of
    --                         Right newGW -> do
    --                             modifyWorld (const newGW)
    --                             msg $ PutItemIn itemTag containerTag
    --                         Left err -> return err
    --                 else msg $ Command.Message.NotAContainer containerTag
    --         | itemTagExistsAtActorLoc itemTag gw ->
    --             msg $ NoContainerForItem itemTag containerTag
    --         | itemTagExistsAtActorLoc containerTag gw ->
    --             msg $ NoItemForContainer itemTag containerTag
    --         | otherwise ->
    --             msg $ InvalidItemInLocation itemTag
    --     (Nothing, _) -> msgGameWordError $ ItemError itemTag
    --     (_, Nothing) -> msgGameWordError $ ItemError containerTag

executePut :: CommandExecutor
executePut expr = do
    gw <- getWorld
    case expr of
        AtomicExpression {} ->
            msg PutWhat
        UnaryExpression _ (NounClause itemTag) ->
            msg $ PutWhere itemTag
        BinaryExpression {} ->
            msg PutWhat
        ComplexExpression _ (NounClause itemTag) _ (NounClause containerTag) ->
            putItemInContainer itemTag containerTag gw
