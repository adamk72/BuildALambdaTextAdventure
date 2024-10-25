{-# LANGUAGE ViewPatterns #-}
module Repl.Parse (parse) where

import qualified Data.Text as T
import Story.Locations (executeLook)

parse :: T.Text -> T.Text
parse s
  | lowerCase == "look around" = executeLook (Just "around")
    where lowerCase = T.toLower s
parse other = "I don't know how to do " <> other <> "."