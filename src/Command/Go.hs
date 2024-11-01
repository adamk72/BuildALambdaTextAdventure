{-# LANGUAGE LambdaCase #-}

module Command.Go (executeGo, GoMessage(..), renderMessage) where

import           Control.Monad.State
import           Core.State          (Character (..), GameWorld (..),
                                      Location (..))
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

executeGo :: Maybe Text -> State GameWorld Text
executeGo target = do
  gw <- get
  let ac = activeCharacter gw
      validLocTags = destinationTags $ currentLocation ac
  case target of
    Just moveTo | moveTo `elem` validLocTags  ->
      case find (\loc -> locTag loc == moveTo) (locations gw) of -- check is a legit location.
        Just newLoc -> do
          let newAc = ac { currentLocation = newLoc }
          put gw { activeCharacter = newAc  }
          return $ renderMessage $ MovingToLocation moveTo
        Nothing -> error $ unpack $ renderMessage $ DoesNotExist moveTo -- this means the JSON file was malformed.
    Just already | already == (locTag $ currentLocation ac) ->
      return $ renderMessage $ AlreadyAtLocation already
    Just noWay -> return $ renderMessage $ NoPath noWay
    Nothing -> return $ renderMessage NoLocationSpecified
