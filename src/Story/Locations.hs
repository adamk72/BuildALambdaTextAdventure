module Story.Locations (executeLook) where

import qualified Data.Text as T

data Location = OpenField | LittleCave

executeLook :: Maybe T.Text -> T.Text
executeLook (Just "around") = "You are somewhere."
executeLook Nothing = "I'm not sure what you want to look at."



