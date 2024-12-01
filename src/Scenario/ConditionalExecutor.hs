module Scenario.ConditionalExecutor (ConditionalExecutor, executeConditionCheck) where

import           Core.State.GameState
import           Data.Text               (Text)
import           Entity.Class.Capacity   (getItemList)
import           Entity.Class.EntityBase (getId)
import           Entity.Class.Viewable   (getLocationId)
import           Entity.Entity           (EntityResult (..), World (..), findEntityById)
import           Entity.Types.Common     (EntityId (..))
import           Parser.Types            (CondExpression (..), PossessionClause (..), StateClause (..), SubjClause (..))

type ConditionalExecutor = CondExpression -> GameMonad Text

-- | Check if an entity has a specific state
checkState :: EntityId -> Text -> World -> Bool
checkState entityId state world =
    case findEntityById entityId world of
        Just (ActorResult _)    -> True  -- Actors can have states (would need state tracking)
        Just (ItemResult _)     -> True  -- Items can have states (would need state tracking)
        Just (LocationResult _) -> True  -- Locations can have states (would need state tracking)
        Nothing                 -> False

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

    PosStateExpression (SubjClause subject) (StateClause state) ->
        checkState (EntityId subject) state world

    NegStateExpression (SubjClause subject) (StateClause state) ->
        not $ checkState (EntityId subject) state world

    PossessiveExpression (SubjClause subject) (PossessionClause itemTag) ->
        case findEntityById (EntityId subject) world of
            Just (ActorResult actor) ->
                let items = getItemList (getId actor) world
                in any (\i -> unEntityId (getId i) == itemTag) items
            Just (ItemResult container) ->
                let items = getItemList (getId container) world
                in any (\i -> unEntityId (getId i) == itemTag) items
            _ -> False

    NonPossessiveExpression (SubjClause subject) (PossessionClause itemTag) ->
        case findEntityById (EntityId subject) world of
            Just (ActorResult actor) ->
                let items = getItemList (getId actor) world
                in not $ any (\i -> unEntityId (getId i) == itemTag) items
            Just (ItemResult container) ->
                let items = getItemList (getId container) world
                in not $ any (\i -> unEntityId (getId i) == itemTag) items
            _ -> False
