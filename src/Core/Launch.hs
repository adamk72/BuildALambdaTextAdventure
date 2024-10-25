module Core.Launch (launch) where

import           Repl
import Control.Monad (unless)

launch :: IO ()
launch = do
    quit <- loop
    unless quit launch
