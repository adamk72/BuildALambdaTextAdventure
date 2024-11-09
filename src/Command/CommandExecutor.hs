module Command.CommandExecutor (CommandExecutor) where

import           Control.Monad.State
import           Core.State.GameState
import           Data.Text
import           Parser.Types

type CommandExecutor = Expression -> State GameWorld Text
