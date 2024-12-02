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
    { actions  :: [CmdExpression]
    , response :: Text
    } deriving (Show, Eq, Generic)

data ConditionType
    = All [CondExpression]
    | Any [CondExpression]
    deriving (Show, Eq, Generic)

data ConditionGroup = ConditionGroup
    { conditions :: ConditionType
    , whileFalse :: Maybe [ScenarioResponse]
    , whileTrue  :: Maybe [ScenarioResponse]
    } deriving (Show, Eq, Generic)
