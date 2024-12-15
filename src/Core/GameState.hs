module Core.GameState (AppState (..), GameMonad, GameState (..), GameStateText, World (..)) where

import           Control.Monad.State
import           Data.Text           (Text)
import           Entity.Entity
import           Logger              (GameHistoryLog)

type GameMonad a = StateT GameState IO a

data GameState = GameState
  { gsWorld      :: World,
    gsHistoryLog :: GameHistoryLog
  }

type GameStateText = GameMonad Text

data AppState = AppState
  { gameWorld      :: World,
    gameHistoryLog :: GameHistoryLog,
    isReplayMode   :: Bool,
    replayCommands :: [Text]
  }
