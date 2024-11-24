module Command.Go (module Command.Go) where

import           Command.CommandExecutor
import           Command.Message
import           Core.GameMonad
import           Core.State.GameState
import           Data.Maybe              (fromMaybe)
import           Data.Text               (Text)
import           Entity.Class.Movable
import           Entity.Entity
import           Entity.Ops.Location     (getLocationDestinations)
import           Parser.Types

moveTo :: Text -> World -> GameStateText
moveTo dstTag gw
    | currLocTag == dstTag = msg $ AlreadyAtLocation dstTag
    | dstId `elem` dstIds = do
        let newAc = setLocationId dstId ac
        modifyWorld $ \w -> w { activeActor = newAc }
        msg $ MovingToLocation dstTag
    | otherwise = msg $ NoPath dstTag
  where
    ac = activeActor gw
    dstId = EntityId dstTag
    currLocTag = unEntityId $ getLocationId ac
    dstIds = fromMaybe [] (getLocationDestinations (getLocationId ac) gw)

executeGo :: CommandExecutor
executeGo expr = do
    gw <- getWorld
    case expr of
        (AtomicExpression _)                     -> msg GoWhere
        (UnaryExpression _ (NounClause dst) )    -> moveTo dst gw
        (BinaryExpression _ _ (NounClause dst) ) -> moveTo dst gw
        _                                        -> msg NotSure
