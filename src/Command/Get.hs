module Command.Get (executeGet) where

import           Command.Executor
import           Command.Message
import           Core.GameMonad
import           Core.State
import           Data.Maybe
import           Entity.Class.Capacity   (changeItemContainer)
import           Entity.Class.EntityBase
import           Entity.Entity
import           Entity.Types.Common     (EntityId (..))
import           Parser.Types
import           Parser.Utils
import           Prelude                 hiding (pred)
import           Utils                   (ItemTag)

getItem :: ItemTag -> World -> GameStateText
getItem itemTag gw =
  case findEntityById itemId gw of
    Just (ItemResult item) | item `elem` itemsInLoc   -> addItemToInventory item
    Just (ItemResult item) | item `elem` itemsOnActor -> msg $ AlreadyHaveItem (getName item)
    _                                                 -> msg $ DontSeeItem itemTag
  where
    itemId = EntityId itemTag
    itemsInLoc = getEntityInventoryList (getActiveActorLocation gw) gw
    itemsOnActor = getActiveActorInventoryList gw

    addItemToInventory :: Entity 'ItemT -> GameStateText
    addItemToInventory item =
      case changeItemContainer (activeActor gw) item gw of
        Right updatedGW -> do
          modifyWorld (const updatedGW)
          msg $ PickedUp itemTag (getName (activeActor gw))
        Left errMsg -> return errMsg

executeGet :: BasicCommandExecutor
executeGet expr = do
  gw <- getWorld
  let handle = \case
        AtomicCmdExpression {} -> msg GetWhat
        UnaryCmdExpression _ (NounClause itemTag) ->
          getItem itemTag gw
        SplitCmdExpression {} -> msg GetWhat
        ComplexCmdExpression _ (NounClause itemTag) (PrepClause prep) _
          | prep `isPrepVariantOf` "from" || prep `isPrepVariantOf` "in" ->
              getItem itemTag gw
        ComplexCmdExpression {} -> return "TBD"
  handle expr
