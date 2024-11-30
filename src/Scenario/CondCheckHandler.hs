module Scenario.CondCheckHandler (CommandHandler (..), allCommands, findCommand, knownCmdVerbs) where

import           Parser.Types
import           Scenario.ConditionalChecks
import           Scenario.ConditionalExecutor

data CondCheckHandler = CondCheckHandler
    { condType :: CondExpression
    , condExec :: ConditionalExecutor
    }

allConditions :: [CondCheckHandler]
allConditions =
    [ CondCheckHandler PosStateExpression executeIs
    , CondCheckHandler NegStateExpression executeIsNot
    , CondCheckHandler PossessiveExpression executeHas
    , CondCheckHandler NonPossessiveExpression executeHasNot
    ]

