{-# LANGUAGE FlexibleInstances #-}
module Scenario.Check (ScenarioCheck (..), executeConditionCheck, handleScenarioCheck) where

import           Command.CommandExecutor (CommandExecutor, ScenarioCheckExecutor (..))
import           Core.GameMonad
import           Core.State.GameState
import qualified Data.Map                as Map
import           Data.Text               (Text)
import           Entity.Class.Capacity   (getItemList)
import           Entity.Class.EntityBase (getId, isOfType)
import           Entity.Class.Viewable   (getLocationId)
import           Entity.Entity           (EntityResult (..), findEntityById)
import           Entity.Types.Common     (EntityId (..), LocationId)
import           Parser.Types
    (CmdExpression, CondExpression (..), PossessionClause (..), StateClause (..), SubjClause (..))
import           Scenario.Types

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


executeConditionCheck :: CondExpression -> World -> Bool
executeConditionCheck expr world = case expr of
    AtLocationExpression (SubjClause subject) (StateClause location) -> checkLocation (EntityId subject) (EntityId location) (==)
    NotAtLocationExpression (SubjClause subject) (StateClause location) -> checkLocation (EntityId subject) (EntityId location) (/=)
    PossessiveExpression (SubjClause subject) (PossessionClause tag) -> checkPossession (EntityId subject) tag id
    NonPossessiveExpression (SubjClause subject) (PossessionClause itemTag) -> checkPossession (EntityId subject) itemTag not
    PosStateExpression (SubjClause subject) (StateClause tag) -> checkForType (EntityId subject) tag id
    NegStateExpression (SubjClause subject) (StateClause tag) -> checkForType (EntityId subject) tag not
  where
    checkLocation :: EntityId -> LocationId -> (LocationId -> LocationId -> Bool) -> Bool
    checkLocation subjectId locId comp = case findEntityById subjectId world of
        Just (ActorResult actor) -> getLocationId actor `comp` locId
        Just (ItemResult item)   -> getLocationId item `comp` locId
        _                        -> False

    checkPossession :: EntityId -> Text -> (Bool -> Bool) -> Bool
    checkPossession subjectId itemTag modifier = case findEntityById subjectId world of
        Just (ActorResult actor)    -> modifier $ hasItem (getId actor) itemTag
        Just (ItemResult container) -> modifier $ hasItem (getId container) itemTag
        _                           -> False

    hasItem :: EntityId -> Text -> Bool
    hasItem containerId tag = any (\i -> unEntityId (getId i) == tag) $
                             getItemList containerId world

    checkForType :: EntityId -> Text ->  (Bool -> Bool) -> Bool
    checkForType entityId targetTag modifier =
        case findEntityById entityId world of
            Just (LocationResult loc) -> modifier $ isOfType loc targetTag
            Just (ActorResult actor)  -> modifier $ isOfType actor targetTag
            Just (ItemResult item)    -> modifier $ isOfType item targetTag
            _                         -> False

