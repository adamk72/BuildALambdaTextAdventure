module Command.Get (module Command.Get) where

import           Command.Common
import           Control.Monad.State
import           Core.State
import           Data.List           (find)
import           Data.Text           (unpack)
import           Parser.Types

executeGet :: CommandExecutor
executeGet expr = do
    gw <- get
    let acLoc = getActiveActorLoc gw
        ac = gwActiveActor gw
        validObjTags = map getTag $ getItemsAtLoc acLoc gw
    case expr of
        (UnaryExpression _ (NounClause pickFrom))  | pickFrom `elem` validObjTags ->
            case find (\item -> getTag item == pickFrom) (gwItems gw) of
                Just foundObj -> do
                    let ps = getActorInventory gw
                        updatedGW = moveItemLoc foundObj ps gw
                    put updatedGW
                    return $ renderMessage $ PickedUp pickFrom (getName ac)
                Nothing -> error $ unpack $ renderMessage $ ItemDoesNotExist pickFrom
        _ -> return $ renderMessage $ InvalidItem "Pending"
