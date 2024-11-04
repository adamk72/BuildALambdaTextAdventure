{-# LANGUAGE LambdaCase #-}
module Command.Go (module Command.Go) where

import Command.Common
import Control.Monad.State
import Core.State
import Data.Text (Text, unpack)
import Data.List (find)

data GoMessage
    = AlreadyAtLocation Text
    | MovingToLocation Text
    | DoesNotExist Text
    | NoLocationSpecified
    | NoPath Text
    deriving (Eq, Show)

instance CommandMessage GoMessage where
    renderMessage = \case
        AlreadyAtLocation loc -> "You're already in " <> loc <> "."
        MovingToLocation loc -> "Moving to " <> loc <> "."
        DoesNotExist loc -> "Location does not exist in this game world: " <> loc <> "."
        NoPath loc -> "There is no indication there's a way to get to " <> loc <> "."
        NoLocationSpecified -> "Unable to find a location at all."

executeGo :: CommandExecutor
executeGo target = do
    gw <- get
    let ac = gwActiveCharacter gw
        validLocTags = destinationTags $ getLocation ac
    case target of
        Just moveTo | moveTo `elem` validLocTags ->
            case find (\loc -> locTag loc == moveTo) (gwLocations gw) of
                Just newLoc -> do
                    let newAc = setCharLoc newLoc ac
                    put gw { gwActiveCharacter = newAc }
                    return $ renderMessage $ MovingToLocation moveTo
                Nothing -> error $ unpack $ renderMessage $ DoesNotExist moveTo
        Just already | already == locTag (getLocation ac) ->
            return $ renderMessage $ AlreadyAtLocation already
        Just noWay -> return $ renderMessage $ NoPath noWay
        Nothing -> return $ renderMessage NoLocationSpecified