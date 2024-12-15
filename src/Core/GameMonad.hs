module Core.GameMonad (getWorld, logGameDebug, logGameError, logGameInfo, modifyWorld) where

import           Control.Monad.State
import           Core.GameState      (GameMonad, GameState (..), World)
import           Data.Text           (Text)
import           Logger

getWorld :: GameMonad World
getWorld = gets gsWorld

modifyWorld :: (World -> World) -> GameMonad ()
modifyWorld f = modify $ \s -> s {gsWorld = f (gsWorld s)}

logGameDebug :: Text -> GameMonad ()
logGameDebug msg = do
  _state <- get
  newHistory <- liftIO $ logDebug (gsHistoryLog _state) msg
  modify $ \s -> s {gsHistoryLog = newHistory}

logGameInfo :: Text -> GameMonad ()
logGameInfo msg = do
  _state <- get
  newHistory <- liftIO $ logInfo (gsHistoryLog _state) msg
  modify $ \s -> s {gsHistoryLog = newHistory}

logGameError :: Text -> GameMonad ()
logGameError msg = do
  _state <- get
  newHistory <- liftIO $ logError (gsHistoryLog _state) msg
  modify $ \s -> s {gsHistoryLog = newHistory}
