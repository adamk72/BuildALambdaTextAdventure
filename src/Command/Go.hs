module Command.Go (module Command.Go) where

import           Command.Common
import           Control.Monad.State
import           Core.State
import           Data.List           (find)
import           Data.Text           (Text, unpack)
import           Parser.Types

moveTo :: Actor -> Text -> GameWorld -> State GameWorld Text
moveTo actor destination gw =
    case find (\loc -> locTag loc == destination) (gwLocations gw) of
        Just newLoc -> do
            let newAc = setActorLoc newLoc actor
            put gw { gwActiveActor = newAc }
            return $ renderMessage $ MovingToLocation destination

executeGo :: CommandExecutor
executeGo expr = do
    gw <- get
    let ac = gwActiveActor gw
        validLocTags = destinationTags $ getLocation ac
    case expr of
        (UnaryExpression _ (NounClause destination) )   | destination `elem` validLocTags -> moveTo ac destination gw
        (BinaryExpression _ _ (NounClause destination) )   | destination `elem` validLocTags -> moveTo ac destination gw
        _ -> return "Pending"
        --         Nothing -> error $ unpack $ renderMessage $ LocationDoesNotExist moveTo
        -- already | already == locTag (getLocation ac) ->
        --     return $ renderMessage $ AlreadyAtLocation already
        -- noWay -> return $ renderMessage $ NoPath noWay
