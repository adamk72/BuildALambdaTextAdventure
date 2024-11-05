{-# LANGUAGE LambdaCase #-}
module Command.Get (module Command.Get) where

import           Command.Common
import           Control.Monad.State
import           Core.State
import           Data.List           (find)
import           Data.Text           (Text, unpack)

data GetMessage
    = PickedUp Text Text
    | InvalidItem Text
    | DoesNotExist Text
    | NoItemSpecified
    deriving (Eq, Show)

instance CommandMessage GetMessage where
    renderMessage = \case
        PickedUp item actor -> "Moved " <> item <> " to " <> actor
        InvalidItem item -> "Cannot pick up " <> item
        NoItemSpecified -> "What do you want to get?"
        DoesNotExist item -> "Item does not exist in this game world: " <> item <> "."

executeGet :: CommandExecutor
executeGet target = do
    gw <- get
    let acLoc = gwActiveActorLoc gw
        ac = gwActiveActor gw
        validObjTags = map getTag $ getItemsAtLocation gw acLoc
    case target of
        Just pickFrom | pickFrom `elem` validObjTags ->
            case find (\item -> getTag item == pickFrom) (gwItems gw) of
                Just foundObj -> do
                    let ps = getPocketSlot gw
                        updatedGW = updateItem
                            (\obj -> obj { entityTag = (entityTag obj) { location = ps } })
                            foundObj
                            gw
                    put updatedGW
                    return $ renderMessage $ PickedUp pickFrom (getName ac)
                Nothing -> error $ unpack $ renderMessage $ DoesNotExist pickFrom
        Just noWay -> return $ renderMessage $ InvalidItem noWay
        Nothing -> return $ renderMessage NoItemSpecified
