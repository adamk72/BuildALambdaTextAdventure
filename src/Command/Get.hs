module Command.Get (module Command.Get) where

import           Command.CommandExecutor
import           Core.GameMonad
import           Core.Message
import           Core.State
import           Data.Text               (Text)
import           Parser.Types
import           Parser.Utils

-- | Attempt to take an item from a specific location or container
getItem :: Text -> Maybe Text -> Entity 'ItemT ->Entity 'ActorT -> World -> GameStateText
getItem itemTag srcM visibleItems actor gw
    | not (tagInItemList itemTag visibleItems) = msg $ InvalidItem itemTag
    | otherwise = case findItemByTag itemTag gw of
        Nothing -> msgGameWordError $ ItemDoesNotExist itemTag
        Just item -> do
            let acInv = getActorInventory gw
            if getLocation item == acInv
            then msg $ AlreadyHaveItem (getName item)
            else tryGetItem item acInv srcM

  where
    tryGetItem :: Entity 'ItemT -> Entity 'LocationT -> Maybe Text -> GameStateText
    tryGetItem item locOrItem = \case
        Nothing -> moveAndMsg item locOrItem
        Just src -> case findAnyLocationByTag src gw of
            Just loc -> if itemExistsAtLoc itemTag loc gw True
                       then moveAndMsg item locOrItem
                       else msg $ NotFoundIn itemTag src
            Nothing -> case findItemByTag src gw of
                Just container -> if isContainer container
                                then moveAndMsg item locOrItem
                                else msg $ NotAContainer src
                Nothing -> msg $ InvalidItem src

    moveAndMsg :: Entity 'ItemT -> Entity 'LocationT -> GameStateText
    moveAndMsg item dstLoc = do
        let updatedGW = moveItemLoc item dstLoc gw
        modifyWorld (const updatedGW)
        msg $ PickedUp itemTag (getName actor)

executeGet :: CommandExecutor
executeGet expr = do
    gw <- getWorld
    let ac = gwActiveActor gw
        acLoc = getActiveActorLoc gw
        visibleItems = getItemsAtLocDeep acLoc gw True
        handle = \case
            AtomicExpression {} -> msg GetWhat
            UnaryExpression _ (NounClause itemTag) ->
                getItem itemTag Nothing visibleItems ac gw
            BinaryExpression {} -> msg GetWhat
            ComplexExpression _ (NounClause itemTag) (PrepClause prep) (NounClause src)
                | prep `isPrepVariantOf` "from" || prep `isPrepVariantOf` "in" ->
                    getItem itemTag (Just src) visibleItems ac gw
            ComplexExpression {} -> return "TBD"
    handle expr