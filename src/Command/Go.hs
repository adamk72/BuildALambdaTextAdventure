module Command.Go (executeGo) where

import           Control.Monad.State
import           Core.State          (Character (..), GameWorld (..),
                                      Location (..))
import           Data.Text           (Text)

validLocs :: [Text]
validLocs = ["cave", "meadow"]

executeGo :: Text -> State GameWorld Text
executeGo location = do
  gw <- get
  let ac = activeCharacter gw
  case location of
    known | known `elem` validLocs  ->
      if locTag (currentLocation ac) == known
      then return $ "You're already in " <> known <> "."
      else return $ "Moving to " <> known <> "."
      -- let newAc = ac { activeCharacter}
      -- let newGw = gw { activeCharacter = }
    _ -> return $ "Unknown location: " <> location <> "."


