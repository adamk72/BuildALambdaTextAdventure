module Command.Go (module Command.Go) where

import           Command.CommandExecutor
import           Core.GameMonad
import           Command.Message
import           Core.State.GameState
import           Entity.Entity
import           Data.Text               (Text)
import           Parser.Types
import Entity.Ops.Location (getLocationDestinations)
import Data.Maybe (fromMaybe)

moveTo :: Entity 'ActorT -> Text -> World -> GameStateText
moveTo ac dstTag gw
    | currLocTag == dstTag = msg $ AlreadyAtLocation dstTag
    | dstId `elem` dstIds = do
        let newAc = setLocationId dstId ac
        modifyWorld $ \w -> w { activeActor = newAc }
        msg $ MovingToLocation dstTag
    | otherwise = msg $ NoPath dstTag
  where
    dstId = EntityId dstTag
    currLocTag = unEntityId $ getLocationId ac
    dstIds = fromMaybe [] (getLocationDestinations (getLocationId ac) gw)

executeGo :: CommandExecutor
executeGo expr = do
    gw <- getWorld
    let ac = activeActor gw
    case expr of
        (AtomicExpression _)                     -> msg GoWhere
        (UnaryExpression _ (NounClause dst) )    -> moveTo ac dst gw
        (BinaryExpression _ _ (NounClause dst) ) -> moveTo ac dst gw
        _                                        -> msg NotSure
