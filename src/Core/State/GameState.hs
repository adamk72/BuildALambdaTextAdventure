module Core.State.GameState (AppState (..), World (..), GameState(..), GameMonad, GameStateText) where

import           Control.Monad.State
import           Entity.Entity
import           Data.Text           (Text)
import           Logger              (GameHistoryLog)

type GameMonad a = StateT GameState IO a

data GameState = GameState
    { gsWorld :: World
    , gsHistoryLog :: GameHistoryLog
    }

type GameStateText = GameMonad Text

data AppState = AppState
    { gameWorld   :: World
    , gameHistoryLog :: GameHistoryLog
    }

