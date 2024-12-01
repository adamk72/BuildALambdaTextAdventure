module Command.Go (module Command.Go) where

import           Command.CommandExecutor
import           Command.Message
import           Core.GameMonad
import           Core.State.GameState
import           Data.Maybe              (fromMaybe)
import           Data.Text               (Text)
import           Entity.Class.Movable
import           Entity.Ops.Location     (getLocationDestinations)
import           Entity.Types.Common
import           Parser.Types
import           Scenario.Check          (checkForScenarioResponse)

moveTo :: Text -> World -> GameStateText
moveTo dstTag gw
    | currLocTag == dstTag = msg $ AlreadyAtLocation dstTag
    | dstId `elem` dstIds = case checkForScenarioResponse expr gw of
        Just response -> return response
        Nothing -> do
            let newAc = setLocationId dstId ac
            modifyWorld $ \w -> w { activeActor = newAc }
            msg $ MovingToLocation dstTag
    | otherwise = msg $ NoPath dstTag
  where
    ac = activeActor gw
    dstId = EntityId dstTag
    currLocTag = unEntityId $ getLocationId ac
    dstIds = fromMaybe [] (getLocationDestinations (getLocationId ac) gw)
    expr = UnaryCmdExpression "go" (NounClause dstTag)

executeGo :: CommandExecutor
executeGo expr = do
    gw <- getWorld
    case expr of
        (AtomicCmdExpression _)                     -> msg GoWhere
        (UnaryCmdExpression _ (NounClause dst) )    -> moveTo dst gw
        (BinaryCmdExpression _ _ (NounClause dst) ) -> moveTo dst gw
        _                                           -> msg NotSure
