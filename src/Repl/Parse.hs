module Repl.Parse (parse) where

import qualified Data.Text as T

parse :: T.Text -> T.Text
parse "look" = "It is very dark in here."
parse other  = "I don't know how to " <> other <> "."
