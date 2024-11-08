module Command.Put (module Command.Put) where

import           Command.Common
import           Control.Monad.State
import           Core.State
import           Data.Text           as T (words)

executePut :: CommandExecutor
executePut target = do
    gw <- get
    let sentence = T.words target
    case sentence of
        [itemTag, "in", containerTag] ->
            case findItemByTag itemTag gw of
                Nothing -> return $ renderMessage $ LocationDoesNotExist itemTag
                Just item -> case findItemByTag containerTag gw of
                        Nothing -> return $ renderMessage $ LocationDoesNotExist containerTag
                        Just container -> case getInventory container of
                            Nothing -> return $ renderMessage $ NotAContainer containerTag
                            Just containerLoc -> do
                                let updatedGW = moveItemLoc item containerLoc gw
                                put updatedGW
                                return $ renderMessage $ PutItemIn itemTag containerTag
        _ -> return $ renderMessage $ DontKnowWhere target
