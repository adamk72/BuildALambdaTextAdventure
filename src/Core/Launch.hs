module Core.Launch (launch) where

import           Control.Monad (unless)
import           Repl.Repl          (loop)

launch :: IO ()
launch = do
    quit <- loop
    unless quit launch
