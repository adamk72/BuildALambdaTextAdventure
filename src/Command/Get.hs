module Command.Get (module Command.Get) where

import           Command.CommandExecutor
import           Command.Message
import           Core.GameMonad
import           Core.State
import           Data.Maybe
import           Data.Text               (Text)
import           Entity.Class.EntityBase
import           Entity.Entity           hiding (getItem)
import           Parser.Types
import           Parser.Utils
import           Prelude                 hiding (pred)

-- | Attempt to take an item from a specific location or container
getItem :: Text -> Maybe Text -> World -> GameStateText
getItem itemTag srcM gw =
    case findEntityById itemId gw of
        Just (ItemResult item) | item `elem` itemsInLoc   -> tryGetItem item
        Just (ItemResult item) | item `elem` itemsOnActor -> msg $ AlreadyHaveItem (getName item)
        _                                                 -> msg $ DontSeeItem itemTag
    where
        itemId = EntityId itemTag
        itemsInLoc = getEntityInventoryList (getActiveActorLocation gw) gw
        itemsOnActor = getActiveActorInventoryList gw

        tryGetItem :: Entity 'ItemT -> GameStateText
        tryGetItem item = do
            let updatedGW = updateLocation (getId (activeActor gw)) item gw
            modifyWorld (const updatedGW)
            msg $ PickedUp itemTag (getName (activeActor gw))

--     | not (isInInventoryOf itemTag visibleItems) = msg $ DontSeeItem itemTag
--     | otherwise = case findEntityById itemTag gw of
--         Nothing -> msgGameWordError $ ItemDoesNotExist itemTag
--         Just item -> do
--             let acInv = getActiveActorInventoryId gw
--             if getLocationId item == acInv
--             then msg $ AlreadyHaveItem (getName item)
--             else tryGetItem item acInv srcM

--   where
--     tryGetItem :: Entity 'ItemT -> Entity 'LocationT -> Maybe Text -> GameStateText
--     tryGetItem item locOrItem = \case
--         Nothing -> moveAndMsg item locOrItem
--         Just src -> case findAnyLocationByTag src gw of
--             Just loc -> if itemExistsAtLoc itemTag loc gw True
--                        then moveAndMsg item locOrItem
--                        else msg $ NotFoundIn itemTag src
--             Nothing -> case findEntityById src gw of
--                 Just container -> if isContainer container
--                                 then moveAndMsg item locOrItem
--                                 else msg $ NotAContainer src
--                 Nothing -> msg $ DontSeeItem src

--     moveAndMsg :: Entity 'ItemT -> Entity 'LocationT -> GameStateText
--     moveAndMsg item dstLoc = do
--         let updatedGW = moveItemLoc item dstLoc gw
--         modifyWorld (const updatedGW)
--         msg $ PickedUp itemTag (getName actor)

executeGet :: CommandExecutor
executeGet expr = do
    gw <- getWorld
    let handle = \case
            AtomicExpression {} -> msg GetWhat
            UnaryExpression _ (NounClause itemTag) ->
                getItem itemTag Nothing gw
            BinaryExpression {} -> msg GetWhat
            ComplexExpression _ (NounClause itemTag) (PrepClause prep) (NounClause src)
                | prep `isPrepVariantOf` "from" || prep `isPrepVariantOf` "in" ->
                    getItem itemTag (Just src) gw
            ComplexExpression {} -> return "TBD"
    handle expr
