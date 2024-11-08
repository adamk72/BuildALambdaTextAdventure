module Command.Get (module Command.Get) where

import           Command.Common
import           Control.Monad.State
import           Core.State
import           Data.List           (find)
import           Data.Text           (unpack)

executeGet :: CommandExecutor
executeGet target = do
    gw <- get
    let acLoc = getActiveActorLoc gw
        ac = gwActiveActor gw
        validObjTags = map getTag $ getItemsAtLoc acLoc gw
    case target of
        pickFrom | pickFrom `elem` validObjTags ->
            case find (\item -> getTag item == pickFrom) (gwItems gw) of
                Just foundObj -> do
                    let ps = getActorInventory gw
                        updatedGW = moveItemLoc foundObj ps gw
                    put updatedGW
                    return $ renderMessage $ PickedUp pickFrom (getName ac)
                Nothing -> error $ unpack $ renderMessage $ ItemDoesNotExist pickFrom
        noWay -> return $ renderMessage $ InvalidItem noWay
