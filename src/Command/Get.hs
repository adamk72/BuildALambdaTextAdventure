module Command.Get (module Command.Get) where

import           Command.Messages
import           Control.Monad.State
import           Core.State
import           Data.Text           (Text)
import           Parser.Types

getItem :: Text -> [Text] -> Actor -> GameWorld -> State GameWorld Text
getItem itemTag validItemTags actor gw
    | itemTag `elem` validItemTags =
        case findItemByTag itemTag gw of
            Just foundObj -> do
                let ps = getActorInventory gw
                    updatedGW = moveItemLoc foundObj ps gw
                put updatedGW
                msg $ PickedUp itemTag (getName actor)
            Nothing -> msgGameWordError $ ItemDoesNotExist itemTag
    | otherwise = msg $ InvalidItem itemTag

executeGet :: CommandExecutor
executeGet expr = do
    gw <- get
    let acLoc = getActiveActorLoc gw
        ac = gwActiveActor gw
        validItemTags = map getTag $ getItemsAtLoc acLoc gw
        handle = \case
            AtomicExpression {} ->
                msg GetWhat
            UnaryExpression _ (NounClause itemTag) ->
                getItem itemTag validItemTags ac gw
            BinaryExpression {} ->
                msg GetWhat
            ComplexExpression _ (NounClause itemTag) (PrepClause _prep) (NounClause _locTag) ->
                getItem itemTag validItemTags ac gw
    handle expr
