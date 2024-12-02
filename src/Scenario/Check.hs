module Scenario.Check (checkForScenarioResponse) where

import Data.Text (Text)
import qualified Data.Map as Map
import Parser.Types (CmdExpression)
import Scenario.Types
import Entity.Entity (World(scenarios))
import Scenario.ConditionalExecutor (executeConditionCheck)

checkConditionGroup :: ConditionGroup -> World -> Bool
checkConditionGroup group world = case conditions group of
    All conds -> all (`executeConditionCheck` world) conds
    Any conds -> any (`executeConditionCheck` world) conds

checkForScenarioResponse :: CmdExpression -> World -> Maybe Text
checkForScenarioResponse cmd world =
    let allScenarios = Map.elems (scenarios world)

        checkScenario scenario = checkConditionGroups (endConditions scenario)

        checkGroup group result = case result of
            Just r -> Just r  -- Already found a response
            Nothing ->
                let conditionMet = checkConditionGroup group world
                    responses = if conditionMet
                            then whileTrue group
                            else whileFalse group
                in case responses of
                    Just rs -> foldr (checkResponse cmd) Nothing rs
                    Nothing -> Nothing

        checkResponse :: CmdExpression -> ScenarioResponse -> Maybe Text -> Maybe Text
        checkResponse userCmd resp result = case result of
            Just r -> Just r  -- Already found a response
            Nothing -> if userCmd `elem` actions resp
                      then Just (response resp)
                      else Nothing

        checkConditionGroups = foldr checkGroup Nothing

    in foldr (\scenario result -> case result of
        Just r -> Just r
        Nothing -> checkScenario scenario) Nothing allScenarios