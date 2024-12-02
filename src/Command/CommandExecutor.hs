{-# LANGUAGE FlexibleInstances #-}
module Command.CommandExecutor (CommandExecutor, ScenarioCheckExecutor (..)) where

import           Core.State.GameState
import           Data.Text
import           Parser.Types

type CommandExecutor = CmdExpression -> GameMonad Text

newtype ScenarioCheckExecutor = ScenarioCheckExecutor
    { runScenarioCheck :: CommandExecutor }

