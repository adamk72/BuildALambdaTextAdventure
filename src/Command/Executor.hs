{-# LANGUAGE FlexibleInstances #-}
module Command.Executor (BasicCommandExecutor, ScenarioCheckExecutor (..)) where

import           Core.State.GameState
import           Data.Text
import           Parser.Types

type BasicCommandExecutor = CmdExpression -> GameMonad Text

newtype ScenarioCheckExecutor = ScenarioCheckExecutor
    { runScenarioCheck :: BasicCommandExecutor }

