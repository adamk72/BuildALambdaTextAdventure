module Core.Launch (launch) where

import           Control.Monad       (unless)
import           Core.State          (GameWorld, initialWorld)
import qualified Repl.Repl           as Repl (loop)

gameLoop :: GameWorld -> IO ()
gameLoop gw = do
    quit <- Repl.loop gw
    unless quit (gameLoop gw)

launch :: IO ()
launch = do gameLoop initialWorld
