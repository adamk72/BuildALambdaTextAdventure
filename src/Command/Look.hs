{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Command.Look (executeLook, renderMessage) where

import           Command.CommandExecutor
import           Command.Message
import           Core.State
import           Data.Maybe
import           Data.Text                (Text, isSuffixOf)
import           Entity.Class.CanMoveSelf
import           Entity.Class.EntityBase
import           Entity.Entity
import           Entity.Types.Common
import           Parser.Types
import           Parser.Utils
import           Scenario.Check           (toScenarioCheck)
import           Utils

lookInContainer :: Text -> World -> GameStateText
lookInContainer containerTag gw = do
    case findItemIdAtActorLoc (EntityId containerTag) gw of
        Nothing -> return $ "Don't see a " <> containerTag <> " to look into." -- Todo: make this smarter for looking "in" a location.
        Just itemId -> case findItemById itemId gw of
            Nothing -> return $ "Don't see a " <> containerTag <> " to look into."
            Just item -> if isContainer item
                            then do
                                let items = getEntityInventoryList item gw
                                msg $ LookIn containerTag (oxfordEntityNames  items)
                            else msg $ NotAContainer containerTag

lookAt :: Text -> World -> GameStateText
lookAt eTag gw = do
    case findEntityIdAtActorLoc (EntityId eTag) gw of
        Nothing -> return $ "Don't see a " <> eTag <> " to look at."
        Just _ -> case fromJust (findEntityById (EntityId eTag) gw) of
                (LocationResult loc) -> return $ "You're looking at " <> getName loc
                (ActorResult actor)  -> return $ "You're looking at " <> getName actor
                (ItemResult item)    -> return $ "You're looking at " <> getName item

executeLook :: ScenarioCheckExecutor
executeLook = toScenarioCheck executeLookRaw

executeLookRaw :: World -> CommandExecutor
executeLookRaw gw expr = do
    case expr of
        AtomicCmdExpression {} ->
            msg $ YouSeeGeneral "A general view of the space and possibly some items."

        UnaryCmdExpression _ (NounClause "around") -> do
            let surroundings = getViewableNamesAtActorLoc gw
                destinationIds = getLocationDestinations (getLocationId (activeActor gw )) gw
            case destinationIds of
                Just dstIds -> do
                    let dstNames = map unEntityId dstIds
                        message = renderMessage (YouAreIn $ getActiveActorLocationName gw ) <> " " <> renderMessage (LookAround (oxfordComma surroundings)) <> " You see exits to: " <> oxfordComma dstNames <> "."
                    return message
                Nothing -> return "You don't see a way out of here!"

        UnaryCmdExpression _ (NounClause target)
            | "inventory" `isSuffixOf` target ->
                return $ showInventoryList gw
            | otherwise ->
                lookAt target gw

        BinaryCmdExpression _ (PrepClause prep) (NounClause target)
            | (prep `isPrepVariantOf` "in" || prep `isPrepVariantOf` "at") && "inventory" `isSuffixOf` target ->
                return $ showInventoryList gw
            | prep `isPrepVariantOf` "in" ->
                lookInContainer target gw
            | prep `isPrepVariantOf` "at" || prep `isPrepVariantOf` "toward" {- Todo: check why this works; it should't-}  ->
                lookAt target gw
            | otherwise ->
                return "TBD"

        ComplexCmdExpression _ (NounClause object) (PrepClause prep) (NounClause target) ->
            return $ "You look at " <> object <> " " <> prep <> " " <> target <> "."
