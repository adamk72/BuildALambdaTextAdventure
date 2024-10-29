module Core.Launch (launch) where

import           Control.Monad (unless)
import           Core.State    (GameEnvironment, initialWorld)
import           Json
import qualified Repl.Repl     as Repl (loop)

gameLoop :: GameEnvironment -> IO ()
gameLoop gw = do
    quit <- Repl.loop gw
    unless quit (gameLoop gw)

launch :: FilePath -> IO ()
-- launch = do gameLoop initialWorld
launch fp =
   loadGameEnvironmentJSON fp >>= print
