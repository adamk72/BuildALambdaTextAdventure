module Command.Go (executeGo) where

import           Control.Monad.State
import           Core.State          (Character (..), GameWorld (..),
                                      Location (..))
import           Data.Text           (Text)

validLocs :: [Text]
validLocs = ["cave", "meadow"]

executeGo :: Maybe Text -> State GameWorld Text
executeGo gotoLoc = do
  gw <- get
  let ac = activeCharacter gw
  case gotoLoc of
    Just known | known `elem` validLocs  ->
      if locTag (currentLocation ac) == known
      then return $ "You're already in " <> known <> "."
      else do
        let newLoc = Location { locTag = known, locName = "test"}
        let newAc = ac { currentLocation = newLoc }
        put gw { activeCharacter = newAc  }
        return $ "Moving to " <> known <> "."
    Just l -> return $ "Unknown location: " <> l <> "."
    Nothing -> return "Unable to find a location at all."