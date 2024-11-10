{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Command.Look (executeLook, renderMessage) where

import           Command.CommandExecutor
import           Core.Message
import           Core.State
import           Data.Text               (Text, isSuffixOf)
import           Parser.Types
import           Parser.Utils
import           Utils

lookIn :: Text -> GameWorld -> GameStateText
lookIn containerTag gw = do
    case getItemInventoryByTag of
        Left err      -> err
        Right message -> message
        where
            getItemInventoryByTag =
                 case findItemByTag containerTag gw of
                    Nothing -> Left $ msg $ NotAContainer containerTag
                    Just validContainer -> case getInventory validContainer of
                        Nothing -> Left $ msg $ NotAContainer containerTag
                        Just containerLoc -> Right $ msg $ LookIn containerTag (oxfordEntityNames (getItemsAtLoc containerLoc gw))

lookInActorInventory :: GameWorld -> Text
lookInActorInventory gw = "Your inventory has: " <> oxfordEntityNames (getActorInventoryItems gw)

executeLook :: CommandExecutor
executeLook expr = do
    gw <- getGameWorld
    let acLoc = getActiveActorLoc gw
        objs = getItemsAtLoc acLoc gw
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
            return $ "You look at " <> target <> "."
        BinaryExpression _ (PrepClause inP) (NounClause container) | inP `isPrepVariantOf` "in" ->
            lookIn container gw
        BinaryExpression {} -> return "TBD"
        ComplexExpression _  (NounClause object) (PrepClause prep) (NounClause target)->
            return $ "You look at " <> object <> " " <> prep <> " " <> target <> "."
