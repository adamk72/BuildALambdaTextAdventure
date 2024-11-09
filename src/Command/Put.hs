module Command.Put (module Command.Put) where

import           Command.Messages
import           Control.Monad.State
import           Core.State
import           Parser.Types

executePut :: CommandExecutor
executePut expr = do
    gw <- get
    case expr of
        (ComplexExpression _ (NounClause itemTag) _ (NounClause containerTag)) ->
            case findItemByTag itemTag gw of
                Nothing -> msg $ NoPath itemTag
                Just item -> case findItemByTag containerTag gw of
                        Nothing -> msg $ NoPath containerTag
                        Just container -> case getInventory container of
                            Nothing -> msg $ NotAContainer containerTag
                            Just containerLoc -> do
                                let updatedGW = moveItemLoc item containerLoc gw
                                put updatedGW
                                msg $ PutItemIn itemTag containerTag
        _ -> msg $ DontKnowWhere "Pending"
