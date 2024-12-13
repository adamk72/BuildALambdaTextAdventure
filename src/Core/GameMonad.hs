module Core.GameMonad (getWorld, logGameDebug, logGameError, logGameInfo, modifyWorld) where

import           Control.Monad.State
import           Core.State.GameState (GameMonad, GameState (..), World)
import           Data.Text            (Text)
import           Logger

runGameMonad :: GameMonad a -> GameState -> IO (a, GameState)
runGameMonad = runStateT

getWorld :: GameMonad World
getWorld = gets gsWorld

modifyWorld :: (World -> World) -> GameMonad ()
modifyWorld f = modify $ \s -> s { gsWorld = f (gsWorld s) }

logGameDebug :: Text -> GameMonad ()
logGameDebug msg = do
    _state <- get
    newHistory <- liftIO $ logDebug (gsHistory _state) msg
    modify $ \s -> s { gsHistory = newHistory }

logGameInfo :: Text -> GameMonad ()
logGameInfo msg = do
    _state <- get
    newHistory <- liftIO $ logInfo (gsHistory _state) msg
    modify $ \s -> s { gsHistory = newHistory }

logGameError :: Text -> GameMonad ()
logGameError msg = do
    _state <- get
    newHistory <- liftIO $ logError (gsHistory _state) msg
    modify $ \s -> s { gsHistory = newHistory }



