module Scenario.ConditionalExecutor (executeConditionCheck, ConditionalExecutor) where

import           Core.State.GameState
import           Data.Text             (Text)
import           Entity.Class.Viewable (getLocationId)
import           Entity.Entity         (EntityResult (..), findEntityById)
import           Entity.Types.Common   (EntityId (..))
import           Parser.Types          (CondExpression (..), StateClause (..), SubjClause (..))

type ConditionalExecutor = CondExpression -> GameMonad Text

executeConditionCheck :: CondExpression -> World -> Bool
executeConditionCheck expr world = case expr of
    AtLocationExpression (SubjClause subject) (StateClause location) ->
        case findEntityById (EntityId subject) world of
            Just (ActorResult actor) -> getLocationId actor == EntityId location
            Just (ItemResult item)   -> getLocationId item == EntityId location
            _                        -> False

    NotAtLocationExpression (SubjClause subject) (StateClause location) ->
        case findEntityById (EntityId subject) world of
            Just (ActorResult actor) -> getLocationId actor /= EntityId location
            Just (ItemResult item)   -> getLocationId item /= EntityId location
            _                        -> False

    PosStateExpression {} -> False
    NegStateExpression {} -> False
    PossessiveExpression {} -> False
    NonPossessiveExpression {} -> False
