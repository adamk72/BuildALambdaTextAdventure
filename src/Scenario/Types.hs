{-# LANGUAGE DeriveGeneric #-}

module Scenario.Types (ConditionGroup (..), ConditionType (..), Scenario (..), ScenarioResponse (..)) where

import           Data.Text           (Text)
import           Entity.Types.Common (EntityId)
import           GHC.Generics        (Generic)
import           Parser.Types        (CmdExpression, CondExpression)

data Scenario = Scenario
    { tag             :: EntityId
    , name            :: Text
    , startConditions :: [ConditionGroup]
    , endConditions   :: [ConditionGroup]
    , actionsOnceTrue :: Maybe [CmdExpression]
    } deriving (Show, Eq, Generic)

data ScenarioResponse = ScenarioResponse
    { actions  :: [CmdExpression]     -- Actions that trigger this response
    , response :: Text       -- Text to show player
    } deriving (Show, Eq, Generic)

data ConditionType
    = All [CondExpression]    -- All conditions must be met
    | Any [CondExpression]    -- Any condition must be met
    deriving (Show, Eq, Generic)

data ConditionGroup = ConditionGroup
    { conditions :: ConditionType
    , whileFalse :: Maybe [ScenarioResponse]  -- Responses while conditions aren't met
    , whileTrue  :: Maybe [ScenarioResponse]  -- Responses while conditions are met
    } deriving (Show, Eq, Generic)
