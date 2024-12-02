module Scenario.CondCheckHandler (CondCheckHandler (..), allConditions) where

import           Scenario.ConditionalChecks
import           Scenario.ConditionalExecutor

newtype CondCheckHandler = CondCheckHandler { condExec :: ConditionalExecutor }

allConditions :: [CondCheckHandler]
allConditions =
    [ CondCheckHandler executeIs
    , CondCheckHandler executeIsNot
    , CondCheckHandler executeHas
    , CondCheckHandler executeHasNot
    ]

