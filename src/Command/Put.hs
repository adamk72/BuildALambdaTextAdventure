{-# LANGUAGE LambdaCase #-}
module Command.Put (module Command.Put) where

import           Command.Common
import           Control.Monad.State
import           Core.State
import           Data.Text           as T (Text, unpack, words)

data PutMessage
    = PutItemIn Text Text
    | DontKnowWhere Text
    | NotAContainer Text
    | DoesNotExist Text
    deriving (Eq, Show)

instance CommandMessage PutMessage where
    renderMessage = \case
        PutItemIn item dst -> item <> "is now in the " <> dst <> "."
        DontKnowWhere item -> "Don't know where to put " <> item <> "."
        DoesNotExist item -> "Don't see a " <> item <> "."
        NotAContainer item -> "The " <> item <> " is not a container."

executePut :: CommandExecutor
executePut target = do
    gw <- get
    let sentence = T.words target
    case sentence of
        [itemTag, "in", containerTag] ->
            case findItemByTag itemTag gw of
                Nothing -> error $ unpack $ renderMessage $ DoesNotExist itemTag
                Just item -> case findItemByTag containerTag gw of
                        Nothing -> error $ unpack $ renderMessage $ DoesNotExist containerTag
                        Just container -> case getInventory container of
                            Nothing -> return $ renderMessage $ NotAContainer containerTag
                            Just containerLoc -> do
                                let updatedGW = moveItemLoc item containerLoc gw
                                put updatedGW
                                return $ renderMessage $ PutItemIn itemTag containerTag
        _ -> return $ renderMessage $ DontKnowWhere target
