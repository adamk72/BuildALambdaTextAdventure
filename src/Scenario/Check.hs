{-# LANGUAGE FlexibleInstances #-}
module Scenario.Check (ScenarioCheck (..), handleScenarioCheck) where

import           Command.CommandExecutor      (ScenarioCheckExecutor (..), CommandExecutor)
import           Core.State.GameState
import qualified Data.Map                     as Map
import           Data.Text                    (Text)
import           Parser.Types                 (CmdExpression)
import           Scenario.ConditionalExecutor (executeConditionCheck)
import           Scenario.Types
import Core.GameMonad

class ScenarioCheck a where
    toScenarioCheck :: a -> ScenarioCheckExecutor

instance ScenarioCheck (World -> CommandExecutor) where
    toScenarioCheck f = ScenarioCheckExecutor $ \expr -> do
        world <- getWorld
        handleScenarioCheck expr world (f world expr)

handleScenarioCheck :: Monad m => CmdExpression -> World -> m Text -> m Text
handleScenarioCheck cmd world fallback = do
    maybe fallback return (checkForScenarioResponse cmd world)

checkConditionGroup :: ConditionGroup -> World -> Bool
checkConditionGroup group world = case conditions group of
    All conds -> all (`executeConditionCheck` world) conds
    Any conds -> any (`executeConditionCheck` world) conds

checkForScenarioResponse :: CmdExpression -> World -> Maybe Text
checkForScenarioResponse cmd world =
    let allScenarios = Map.elems (scenarios world)

        checkScenario scenario = checkConditionGroups (endConditions scenario)

        checkGroup group result = case result of
            Just r -> Just r
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
            Just r -> Just r
            Nothing -> if userCmd `elem` actions resp
                      then Just (response resp)
                      else Nothing

        checkConditionGroups = foldr checkGroup Nothing

    in foldr (\scenario result -> case result of
        Just r  -> Just r
        Nothing -> checkScenario scenario) Nothing allScenarios
