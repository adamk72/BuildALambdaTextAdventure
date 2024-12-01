module Scenario.Check (checkForScenarioResponse) where

import Data.Text (Text)
import qualified Data.Map as Map
import Parser.Types (CmdExpression)
import Scenario.Types
import Entity.Entity (World(scenarios))

checkForScenarioResponse :: CmdExpression -> World -> Maybe Text
checkForScenarioResponse cmd world =
    let allScenarios = Map.elems (scenarios world)
        checkScenario scenario = checkConditionGroups (endConditions scenario)
        checkConditionGroups groups = foldr checkGroup Nothing groups

        checkGroup :: ConditionGroup -> Maybe Text -> Maybe Text
        checkGroup group result = case result of
            Just r -> Just r  -- Already found a response
            Nothing -> case whileFalse group of
                Nothing -> Nothing
                Just responses -> foldr checkResponse Nothing responses

        checkResponse :: ScenarioResponse -> Maybe Text -> Maybe Text
        checkResponse resp result = case result of
            Just r -> Just r
            Nothing -> if cmd `elem` actions resp
                      then Just (response resp)
                      else Nothing

    in foldr (\scenario result -> case result of
        Just r -> Just r
        Nothing -> checkScenario scenario) Nothing allScenarios