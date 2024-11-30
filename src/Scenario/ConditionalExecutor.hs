module Scenario.ConditionalExecutor (ConditionalExecutor) where


import           Core.State.GameState
import           Data.Text
import           Parser.Types

type ConditionalExecutor = CondExpression -> GameMonad Text