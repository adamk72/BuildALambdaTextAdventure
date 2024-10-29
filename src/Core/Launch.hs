module Core.Launch (launch) where

import           Control.Monad       (unless)
import           Core.State          (Character, initialWorld)
import qualified Repl.Repl           as Repl (loop)

gameLoop :: Character -> IO ()
gameLoop p = do
    quit <- Repl.loop p
    unless quit (gameLoop p)

launch :: IO ()
launch = do gameLoop initialWorld
