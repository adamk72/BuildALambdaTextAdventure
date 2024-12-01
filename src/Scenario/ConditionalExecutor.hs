module Scenario.ConditionalExecutor (ConditionalExecutor, executeConditionCheck) where

import           Core.State.GameState
import           Data.Text               (Text)
import           Entity.Class.Capacity   (getItemList)
import           Entity.Class.EntityBase (getId)
import           Entity.Class.Viewable   (getLocationId)
import           Entity.Entity           (EntityResult (..), findEntityById)
import           Entity.Types.Common     (EntityId (..), LocationId)
import           Parser.Types            (CondExpression (..), PossessionClause (..), StateClause (..), SubjClause (..))

type ConditionalExecutor = CondExpression -> GameMonad Text

executeConditionCheck :: CondExpression -> World -> Bool
executeConditionCheck expr world = case expr of
    AtLocationExpression (SubjClause subject) (StateClause location) ->
        checkLocation (EntityId subject) (EntityId location) (==)

    NotAtLocationExpression (SubjClause subject) (StateClause location) ->
        checkLocation (EntityId subject) (EntityId location) (/=)

    PossessiveExpression (SubjClause subject) (PossessionClause itemTag) ->
        checkPossession (EntityId subject) itemTag id

    NonPossessiveExpression (SubjClause subject) (PossessionClause itemTag) ->
        checkPossession (EntityId subject) itemTag not

    PosStateExpression {} -> False
    NegStateExpression {} -> False
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
