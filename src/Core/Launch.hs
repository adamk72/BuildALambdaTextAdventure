module Core.Launch (launch) where

import           Control.Monad (unless)
import           Core.State    (GameEnvironment, loadGameEnvironmentJSON)
import qualified Repl.Repl     as Repl (loop)


gameLoop :: GameEnvironment -> IO ()
gameLoop gw = do
    quit <- Repl.loop gw
    unless quit (gameLoop gw)

launch :: FilePath -> IO ()
-- launch = do gameLoop initialWorld
launch fp = do
   adventure <- loadGameEnvironmentJSON fp
   case adventure of
    Right a -> print a
    Left e -> print e
