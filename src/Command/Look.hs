{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Command.Look (executeLook, renderMessage) where

import           Command.CommandExecutor
import           Command.Message
import           Core.State
import           Data.Text                  (Text, isSuffixOf)
import           Parser.Types
import           Parser.Utils
import           Utils
import Entity.Entity
import Data.Maybe

-- | Look inside a container
lookInContainer :: Text -> World -> GameStateText
lookInContainer containerTag gw = undefined
    -- case findEntityById containerTag gw of
    --     Nothing -> msg $ ItemDoesNotExist containerTag
    --     Just container ->
    --         if isContainer container
    --         then do
    --             let items = getInventory container gw
    --             msg $ LookIn containerTag (oxfordEntityNames items)
    --         else msg $ NotAContainer containerTag

-- | Look at a specific item or entity
lookAt :: Text -> World -> GameStateText
lookAt eTag gw = do
    let locId  =  getActiveActorLocationId gw
    -- Todo: look only specific items in location
    case findEntityById (EntityId eTag) gw of
        Nothing -> return $ "Don't see a " <> eTag <> " to look at."
        Just (LocResult loc) -> return $ "You're looking at " <> getName loc
        Just (ActorResult actor) -> return $ "You're looking at " <> getName actor
        Just (ItemResult item) -> return $ "You're looking at " <> getName item


    -- do
    -- case findEntityByTagAtLoc eTag loc gw of
    --     Just entity -> return $ "You see " <> getName entity <> "."
    --     Nothing -> do
    --         -- Check inventory
    --         case findItemByTagInActorInventory eTag gw of
    --             Just item -> return $ "In your inventory, you see " <> toLower (getName item) <> "."
    --             Nothing -> do
    --                 -- Check visible containers
    --                 let visibleContainers = filter isContainer $
    --                         map snd $ findAllInstances eTag gw
    --                 case visibleContainers of
    --                     (container:_) ->
    --                         let items = getInventory container gw
    --                         in msg $ LookIn (getId container) (oxfordEntityNames items)
    --                     [] -> return $ "Don't see a " <> eTag <> " to look at."

-- | Look at inventory contents
lookInActorInventory :: World -> Text
lookInActorInventory gw =
    "Your inventory has: " <> oxfordEntityNames (getActiveActorInventoryList gw)

executeLook :: CommandExecutor
executeLook expr = do
    gw <- getWorld
    case expr of
        AtomicExpression {} ->
            msg $ YouSeeGeneral "A general view of the space and possibly some items."

        UnaryExpression _ (NounClause "around") -> do
            let surroundings = getActorVisibleMovablesAtLoc gw
            msg2 (YouAreIn $ getActiveActorLocationName gw ) (LookAround (oxfordComma surroundings))

        UnaryExpression _ (NounClause invClause)
            | "inventory" `isSuffixOf` invClause ->
                return $ lookInActorInventory gw
            | otherwise ->
                lookAt invClause gw

        BinaryExpression _ (PrepClause prep) (NounClause target)
            | prep `isPrepVariantOf` "in" && target == "inventory" ->
                return $ lookInActorInventory gw
            | prep `isPrepVariantOf` "in" ->
                lookInContainer target gw
            | prep `isPrepVariantOf` "at" || prep `isPrepVariantOf` "toward" ->
                lookAt target gw
            | otherwise ->
                return "TBD"

        ComplexExpression _ (NounClause object) (PrepClause prep) (NounClause target) ->
            return $ "You look at " <> object <> " " <> prep <> " " <> target <> "."
