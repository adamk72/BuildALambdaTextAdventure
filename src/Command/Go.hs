module Command.Go (module Command.Go) where

import           Command.CommandExecutor
import           Core.GameMonad
import           Core.Message
import           Core.State
import           Data.List               (find)
import           Data.Text               (Text, unpack)
import           Parser.Types

moveTo ::Entity 'ActorT -> Text -> [Text] -> World -> GameStateText
moveTo actor dstTag validDstTags  gw
    | dstTag == locTag (getLocation actor) = msg $ AlreadyAtLocation $ locTag (getLocation actor)
    | dstTag `elem` validDstTags =
        case find (\loc -> locTag loc == dstTag) (gwLocations gw) of
            Just newLoc -> do
                let newAc = setActorLoc newLoc actor
                modifyWorld (const gw {gwActiveActor = newAc})
                msg $ MovingToLocation dstTag
            Nothing -> error $ unpack $ renderMessage $ LocationError dstTag
    | otherwise = msg $ NoPath dstTag

executeGo :: CommandExecutor
executeGo expr = do
    gw <- getWorld
    let ac = gwActiveActor gw
        validDstTags = destinationTags (getLocation ac)
    case expr of
        (AtomicExpression _)                     -> msg GoWhere
        (UnaryExpression _ (NounClause dst) )    -> moveTo ac dst validDstTags gw
        (BinaryExpression _ _ (NounClause dst) ) -> moveTo ac dst validDstTags gw
        _                                        -> msg NotSure
