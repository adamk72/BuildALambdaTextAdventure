module Command.Get (module Command.Get) where

import           Command.CommandExecutor
import           Command.Message
import           Core.GameMonad
import           Core.State
import           Data.Maybe
import           Data.Text               (Text)
import           Entity.Class.Capacity   (addItem)
import           Entity.Class.EntityBase
import           Entity.Entity           hiding (getItem)
import           Parser.Types
import           Parser.Utils
import           Prelude                 hiding (pred)

-- | Attempt to take an item from a specific location or container
getItem :: Text -> Maybe Text -> World -> GameStateText
getItem itemTag srcM gw =
    case findEntityById itemId gw of
        Just (ItemResult item) | item `elem` itemsInLoc   -> addItemToInventory item
        Just (ItemResult item) | item `elem` itemsOnActor -> msg $ AlreadyHaveItem (getName item)
        _                                                 -> msg $ DontSeeItem itemTag
    where
        itemId = EntityId itemTag
        itemsInLoc = getEntityInventoryList (getActiveActorLocation gw) gw
        itemsOnActor = getActiveActorInventoryList gw

        addItemToInventory :: Entity 'ItemT -> GameStateText
        addItemToInventory item = do
            case addItem (activeActor gw) item gw of
                Right updatedGW -> do
                     modifyWorld (const updatedGW)
                     msg $ PickedUp itemTag (getName (activeActor gw))
                Left errMsg -> return errMsg

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
