module Command.Go (module Command.Go) where

import           Command.Messages
import           Control.Monad.State
import           Core.State
import           Data.List           (find)
import           Data.Text           (Text, unpack)
import           Parser.Types

moveTo :: Actor -> Text -> [Text] -> GameWorld -> State GameWorld Text
moveTo actor dstTag validDstTags  gw
    | dstTag == locTag (getLocation actor) = return $ renderMessage $ AlreadyAtLocation $ locTag (getLocation actor)
    | dstTag `elem` validDstTags =
        case find (\loc -> locTag loc == dstTag) (gwLocations gw) of
            Just newLoc -> do
                let newAc = setActorLoc newLoc actor
                put gw { gwActiveActor = newAc }
                return $ renderMessage $ MovingToLocation dstTag
            Nothing -> error $ unpack $ renderMessage $ LocationError dstTag
    | otherwise = return $ renderMessage $ NoPath dstTag

executeGo :: CommandExecutor
executeGo expr = do
    gw <- get
    let ac = gwActiveActor gw
        validDstTags = destinationTags (getLocation ac)
    case expr of
        (AtomicExpression _) -> return $ renderMessage GoWhere
        (UnaryExpression _ (NounClause dst) ) -> moveTo ac dst validDstTags gw
        (BinaryExpression _ _ (NounClause dst) ) -> moveTo ac dst validDstTags gw
        _ -> return $ renderMessage NotSure
        -- noWay -> return $ renderMessage $ NoPath noWay
