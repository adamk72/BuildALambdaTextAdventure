module Command.CommandExecutor (CommandExecutor) where


import           Core.State.GameState
import           Data.Text
import           Parser.Types

type CommandExecutor = CmdExpression -> GameMonad Text
