module Core.Launch (launch) where

import           Control.Monad       (unless)
import           Core.State          (Player, initialPlayer)
import qualified Repl.Repl           as Repl (loop)

gameLoop :: Player -> IO ()
gameLoop p = do
    quit <- Repl.loop p
    unless quit (gameLoop p)

launch :: IO ()
launch = do gameLoop initialPlayer
