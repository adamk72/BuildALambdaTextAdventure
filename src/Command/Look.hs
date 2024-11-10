{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Command.Look (executeLook, renderMessage) where

import           Command.CommandExecutor
import           Core.Message
import           Core.State
import           Data.Text               (Text, isSuffixOf, toLower)
import           Parser.Types
import           Parser.Utils
import           Utils

lookIn :: Text -> GameWorld -> GameStateText
lookIn containerTag gw = do
    case findItemByTag containerTag gw of
        Nothing -> do
            msg $ NotAContainer containerTag  -- Using renderMessage instead of msg
        Just validContainer ->
            case getInventory validContainer of
                Nothing -> do
                    msg $ NotAContainer containerTag  -- Using renderMessage instead of msg
                Just containerLoc -> do
                    let items = getItemsAtLoc containerLoc gw
                    logGameDebug $ "Container location: " <> intToText (length items)
                    msg $ LookIn containerTag (oxfordEntityNames items)

lookAt :: Text -> Location -> GameWorld -> GameStateText
lookAt eTag loc gw = do
    case findEntityByTagAtLoc eTag loc gw of
        Just entity -> return $ "You see " <> getName entity <> "."
        Nothing -> do
            case findItemByTagInActorInventory eTag gw of
                Nothing -> return $ "Don't see a " <> eTag <> " to look at."
                Just item -> return $ "In your inventory, you see " <> toLower (getName item) <> "."

lookInActorInventory :: GameWorld -> Text
lookInActorInventory gw = "Your inventory has: " <> oxfordEntityNames (getActorInventoryItems gw)

executeLook :: CommandExecutor
executeLook expr = do
    gw <- getGameWorld
    let acLoc = getActiveActorLoc gw
        objs = getEntitiesAtLoc acLoc gw
    case expr of
        AtomicExpression {} ->
            msg $ YouSeeGeneral "A general view of the space and possibly some items"
        UnaryExpression _ (NounClause "around") ->
            msg2 (YouAreIn $ locName acLoc) (LookAround objs)
        UnaryExpression _ (NounClause invClause) | "inventory" `isSuffixOf` invClause ->
            return $ lookInActorInventory gw
        UnaryExpression {} -> return "TBD"
        BinaryExpression _ _ (NounClause invClause) | "inventory" `isSuffixOf` invClause ->
            return $ lookInActorInventory gw
        BinaryExpression _ (PrepClause atP) (NounClause target) | atP `isPrepVariantOf` "at" || atP `isPrepVariantOf` "toward"  ->
            lookAt target acLoc gw
        BinaryExpression _ (PrepClause inP) (NounClause container) | inP `isPrepVariantOf` "in" ->
            lookIn container gw
        BinaryExpression {} -> return "TBD"
        ComplexExpression _  (NounClause object) (PrepClause prep) (NounClause target)->
            return $ "You look at " <> object <> " " <> prep <> " " <> target <> "."
