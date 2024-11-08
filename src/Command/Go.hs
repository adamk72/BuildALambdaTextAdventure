module Command.Go (module Command.Go) where

import           Command.Common
import           Control.Monad.State
import           Core.State
import           Data.List           (find)
import           Data.Text           (unpack)
import           Parser.Types

executeGo :: CommandExecutor
executeGo expr = do
    gw <- get
    let ac = gwActiveActor gw
        validLocTags = destinationTags $ getLocation ac
    case expr of
        (UnaryExpression _ (NounClause moveTo) ) | moveTo `elem` validLocTags ->
            case find (\loc -> locTag loc == moveTo) (gwLocations gw) of
                Just newLoc -> do
                    let newAc = setActorLoc newLoc ac
                    put gw { gwActiveActor = newAc }
                    return $ renderMessage $ MovingToLocation moveTo
        _ -> return "Pending"
        --         Nothing -> error $ unpack $ renderMessage $ LocationDoesNotExist moveTo
        -- already | already == locTag (getLocation ac) ->
        --     return $ renderMessage $ AlreadyAtLocation already
        -- noWay -> return $ renderMessage $ NoPath noWay
