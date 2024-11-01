{-# LANGUAGE LambdaCase #-}

module Command.Go (executeGo, GoMessage(..), renderMessage) where

import           Control.Monad.State
import           Core.State          (Character (..), GameWorld (..),
                                      Location (..))
import           Data.List           (find)
import           Data.Text           (Text)

data GoMessage
  = AlreadyAtLocation Text
  | MovingToLocation Text
  | UnknownLocation Text
  | NoLocationSpecified
  deriving (Eq, Show)

renderMessage :: GoMessage -> Text
renderMessage = \case
  AlreadyAtLocation loc -> "You're already in " <> loc <> "."
  MovingToLocation loc -> "Moving to " <> loc <> "."
  UnknownLocation loc -> "Unknown location: " <> loc <> "."
  NoLocationSpecified -> "Unable to find a location at all."

executeGo :: Maybe Text -> State GameWorld Text
executeGo gotoLoc = do
  gw <- get
  let ac = activeCharacter gw
      validLocs = locations gw
  case gotoLoc of
    Just targetLocTag | targetLocTag `elem` map locTag validLocs  ->
      if locTag (currentLocation ac) == targetLocTag
      then return $ renderMessage $ AlreadyAtLocation targetLocTag
      else do
        case find (\loc -> locTag loc == targetLocTag) validLocs of
          Just newLoc -> do
            let newAc = ac { currentLocation = newLoc }
            put gw { activeCharacter = newAc  }
            return $ renderMessage $ MovingToLocation targetLocTag
          Nothing -> return $ renderMessage $ UnknownLocation targetLocTag
    Just unknown -> return $ renderMessage $ UnknownLocation unknown
    Nothing -> return $ renderMessage NoLocationSpecified
