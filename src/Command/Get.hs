module Command.Get (module Command.Get) where

import Data.Text (Text)
import Data.List (find)
import Control.Monad.State
import Core.State
import Data.Maybe

executeGet :: Maybe Text -> State GameWorld Text
executeGet target = do
  gw <- get
  let acLoc = getActiveCharLoc gw
      ac = gwActiveCharacter gw
      validObjTags = map getTag (Prelude.filter (\inter -> getLocation inter == acLoc) $ gwInteractables gw)
  case target of
    Just pickFrom | pickFrom `elem` validObjTags ->
      case find (\inter -> getTag inter == pickFrom) (gwInteractables gw) of
        Just foundObj -> do
          let pocketSlot = findLocationInInventory (getTag ac) (gwActiveCharacter gw)
              ps = fromJust pocketSlot
              updatedGW = updateInteractable
                (\obj -> obj { entityTag = (entityTag obj) { location = ps } })
                foundObj
                gw
          put updatedGW
          return $ "Moved " <> pickFrom <> " to " <> getName ac
        Nothing -> error "Error in finding something"
    Just noWay -> return $ "ouch" <> noWay
    Nothing -> return "doubleOuch"