module Command.Go (executeGo) where

import           Control.Monad.State
import           Core.State          (Character (..), GameWorld (..),
                                      Location (..))
import           Data.Text           (Text)
import Data.List (find)

executeGo :: Maybe Text -> State GameWorld Text
executeGo gotoLoc = do
  gw <- get
  let ac = activeCharacter gw
      validLocs = locations gw
  case gotoLoc of
    Just targetLocTag | targetLocTag `elem` map locTag validLocs  ->
      if locTag (currentLocation ac) == targetLocTag
      then return $ "You're already in " <> targetLocTag <> "."
      else do
        case find (\loc -> locTag loc == targetLocTag) validLocs of
          Just newLoc -> do
            let newAc = ac { currentLocation = newLoc }
            put gw { activeCharacter = newAc  }
            return $ "Moving to " <> targetLocTag <> "."
          Nothing -> return "Unable to find that location in this world."
    Just l -> return $ "Unknown location: " <> l <> "."
    Nothing -> return "Unable to find a location at all."