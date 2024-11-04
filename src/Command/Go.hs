{-# LANGUAGE LambdaCase #-}

module Command.Go (executeGo, GoMessage(..), renderMessage) where

import           Control.Monad.State
import           Core.State

import           Data.List           (find)
import           Data.Text           (Text, unpack)

data GoMessage
  = AlreadyAtLocation Text
  | MovingToLocation Text
  | DoesNotExist Text
  | NoLocationSpecified
  | NoPath Text
  deriving (Eq, Show)

renderMessage :: GoMessage -> Text
renderMessage = \case
  AlreadyAtLocation loc -> "You're already in " <> loc <> "."
  MovingToLocation loc -> "Moving to " <> loc <> "."
  DoesNotExist loc -> "Location does not exist in this game world: " <> loc <> "."
  NoPath loc -> "There is no indication there's a way to get to " <> loc <> "."
  NoLocationSpecified -> "Unable to find a location at all."

executeGo :: Maybe Text -> State Core.State.GameWorld Text
executeGo target = do
  gw <- get
  let ac = gwActiveCharacter gw
      validLocTags = destinationTags $ Core.State.getLocation ac
  case target of
    Just moveTo | moveTo `elem` validLocTags  ->
      case find (\loc -> locTag loc == moveTo) (gwLocations gw) of -- check it's legit and get the full Location.
        Just newLoc -> do
          let newAc = Core.State.setCharLoc newLoc ac
          put gw { gwActiveCharacter = newAc  }
          return $ renderMessage $ MovingToLocation moveTo
        Nothing -> error $ unpack $ renderMessage $ DoesNotExist moveTo -- this means the JSON file was malformed.
    Just already | already == locTag (Core.State.getLocation ac) ->
      return $ renderMessage $ AlreadyAtLocation already
    Just noWay -> return $ renderMessage $ NoPath noWay
    Nothing -> return $ renderMessage NoLocationSpecified
