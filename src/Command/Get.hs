module Command.Get (module Command.Get) where

import           Command.CommandExecutor
import           Core.GameMonad

import           Core.Message
import           Core.State
import           Data.Text               (Text)
import           Parser.Types
import           Parser.Utils

getItem :: Text -> Maybe Text -> [Item] -> Actor -> GameWorld -> GameStateText
getItem itemTag maybeContTag visibleItems actor gw
    | tagInItemList itemTag visibleItems =
        case findItemByTag itemTag gw of
            Just validItem -> do
                let objLoc = getLocation validItem
                    acInv = getActorInventory gw
                if objLoc == acInv -- Todo: also check if inside of a container in the actor's inventory
                then return $ "You already have " <> getName validItem
                else do
                    case maybeContTag of
                        Just containerOrLoc ->
                            case findAnyLocationByTag containerOrLoc gw of
                                Just locFound -> do
                                    if itemExistsAtLoc itemTag locFound gw True
                                    then do
                                        let updatedGW = moveItemLoc validItem acInv gw
                                        modifyGameWorld (const updatedGW)
                                        msg $ PickedUp itemTag (getName actor)
                                    else return $ "That can't be found in " <> containerOrLoc
                                Nothing -> do
                                    case findItemByTag containerOrLoc gw of
                                        Just containerFound -> do
                                            if isContainer containerFound
                                            then do
                                                let updatedGW = moveItemLoc validItem acInv gw
                                                modifyGameWorld (const updatedGW)
                                                msg $ PickedUp itemTag (getName actor)
                                            else return $ containerOrLoc <> " isn't a container."
                                        Nothing -> msg $ InvalidItem containerOrLoc
                        Nothing -> do
                            let updatedGW = moveItemLoc validItem acInv gw
                            modifyGameWorld (const updatedGW)
                            msg $ PickedUp itemTag (getName actor)
            Nothing -> msgGameWordError $ ItemDoesNotExist itemTag
    | otherwise = msg $ InvalidItem itemTag

executeGet :: CommandExecutor
executeGet expr = do
    gw <- getGameWorld
    let ac = gwActiveActor gw
        acLoc = getActiveActorLoc gw
        visibleItems = getItemsAtLocDeep acLoc gw True
        handle = \case
            AtomicExpression {} ->
                msg GetWhat
            UnaryExpression _ (NounClause itemTag) ->
                getItem itemTag Nothing visibleItems ac gw
            BinaryExpression {} ->
                msg GetWhat
            ComplexExpression _ (NounClause itemTag) (PrepClause prep) (NounClause containerTag)
                | prep `isPrepVariantOf` "from" ||  prep `isPrepVariantOf` "in" ->
                    getItem itemTag (Just containerTag) visibleItems ac gw
            ComplexExpression {} ->
                return "TBD"
    handle expr
