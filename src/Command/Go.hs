module Command.Go (module Command.Go) where

import           Command.CommandExecutor
import           Core.GameMonad
import           Command.Message
import           Core.State.GameState
import           Entity.Entity
import           Data.Text               (Text)
import           Parser.Types

moveTo :: Entity 'ActorT -> Text -> World -> GameStateText
moveTo actor dstTag gw = undefined
    -- | dstTag == locTag (getLocationId actor) = msg $ AlreadyAtLocation $ locTag (getLocationId actor)
    -- | dstTag `elem` validDstTags =
    --     case find (\loc -> locTag loc == dstTag) (gwLocations gw) of
    --         Just newLoc -> do
    --             let newAc = setActorLoc newLoc actor
    --             modifyWorld (const gw {gwActiveActor = newAc})
    --             msg $ MovingToLocation dstTag
    --         Nothing -> error $ unpack $ renderMessage $ LocationError dstTag
    -- | otherwise = msg $ NoPath dstTag

executeGo :: CommandExecutor
executeGo expr = do
    gw <- getWorld
    case expr of
        (AtomicExpression _)                     -> msg GoWhere
        (UnaryExpression _ (NounClause dst) )    -> moveTo (activeActor gw) dst gw
        (BinaryExpression _ _ (NounClause dst) ) -> moveTo (activeActor gw) dst gw
        _                                        -> msg NotSure
