module Scenario.CondCheckHandler (CondCheckHandler (..), allConditions) where

import           Parser.Types
import           Scenario.ConditionalChecks
import           Scenario.ConditionalExecutor

data CondCheckHandler = CondCheckHandler
    { -- condType :: CondExpression
     condExec :: ConditionalExecutor
    }

allConditions :: [CondCheckHandler]
allConditions =
    [ CondCheckHandler executeIs
    , CondCheckHandler executeIsNot
    , CondCheckHandler executeHas
    , CondCheckHandler executeHasNot
    ]

