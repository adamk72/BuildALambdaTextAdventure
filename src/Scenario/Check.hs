{-# LANGUAGE OverloadedStrings #-}
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

        checkConditionGroups groups = foldr checkGroup Nothing groups

        checkGroup group result = case result of
            Just r -> Just r
            Nothing ->
                if not (checkConditionGroup group world)
                then case whileFalse group of
                    Just responses -> foldr checkResponse Nothing responses
                    Nothing -> Nothing
                else case whileTrue group of
                    Just responses -> foldr checkResponse Nothing responses
                    Nothing -> Nothing

        checkResponse resp result = case result of
            Just r -> Just r
            Nothing -> if cmd `elem` actions resp
                      then Just (response resp)
                      else Nothing

    in foldr (\scenario result -> case result of
        Just r -> Just r
        Nothing -> checkScenario scenario) Nothing allScenarios