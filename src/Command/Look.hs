{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Command.Look (executeLook, renderMessage) where

import           Command.CommandExecutor
import           Command.Message
import           Core.State
import           Data.Maybe
import           Data.Text               (Text, isSuffixOf)
import           Entity.Class.EntityBase
import           Entity.Class.Movable
import           Entity.Entity
import           Parser.Types
import           Parser.Utils
import           Utils

-- | Look inside a container
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

-- | Look at a specific item or entity
lookAt :: Text -> World -> GameStateText
lookAt eTag gw = do
    case findEntityIdAtActorLoc (EntityId eTag) gw of
        Nothing -> return $ "Don't see a " <> eTag <> " to look at."
        Just _ -> case fromJust (findEntityById (EntityId eTag) gw) of
                (LocationResult loc)     -> return $ "You're looking at " <> getName loc
                (ActorResult actor) -> return $ "You're looking at " <> getName actor
                (ItemResult item)   -> return $ "You're looking at " <> getName item

-- | Look at inventory contents
lookInActorInventory :: World -> Text
lookInActorInventory gw = do
    let objects = getActiveActorInventoryList gw
    if null objects
        then "You don't have anything in your inventory."
        else "Your inventory has: " <> oxfordEntityNames objects

executeLook :: CommandExecutor
executeLook expr = do
    gw <- getWorld
    case expr of
        AtomicExpression {} ->
            msg $ YouSeeGeneral "A general view of the space and possibly some items."

        UnaryExpression _ (NounClause "around") -> do
            let surroundings = getViewableNamesAtActorLoc gw
                destinationIds = getLocationDestinations (getLocationId (activeActor gw )) gw
            case destinationIds of
                Just dstIds -> do
                    let dstNames = map unEntityId dstIds
                        message = renderMessage (YouAreIn $ getActiveActorLocationName gw ) <> " " <> renderMessage (LookAround (oxfordComma surroundings)) <> " You see exits to: " <> oxfordComma dstNames <> "."
                    return message
                Nothing -> return "You don't see a way out of here!"

        UnaryExpression _ (NounClause target)
            | "inventory" `isSuffixOf` target ->
                return $ lookInActorInventory gw
            | otherwise ->
                lookAt target gw

        BinaryExpression _ (PrepClause prep) (NounClause target)
            | (prep `isPrepVariantOf` "in" || prep `isPrepVariantOf` "at") && "inventory" `isSuffixOf` target ->
                return $ lookInActorInventory gw
            | prep `isPrepVariantOf` "in" ->
                lookInContainer target gw
            | prep `isPrepVariantOf` "at" || prep `isPrepVariantOf` "toward" ->
                lookAt target gw
            | otherwise ->
                return "TBD"

        ComplexExpression _ (NounClause object) (PrepClause prep) (NounClause target) ->
            return $ "You look at " <> object <> " " <> prep <> " " <> target <> "."
