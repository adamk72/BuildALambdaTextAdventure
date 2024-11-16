{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Command.Look (executeLook, renderMessage) where

import           Command.CommandExecutor
import           Core.Message
import           Core.State
import           Data.Text                  (Text, isSuffixOf, toLower)
import           Parser.Types
import           Parser.Utils
import           Utils

-- | Look inside a container
lookInContainer :: Text -> World -> GameStateText
lookInContainer containerTag gw = undefined
    -- case findEntityById containerTag gw of
    --     Nothing -> msg $ ItemDoesNotExist containerTag
    --     Just container ->
    --         if isContainer container
    --         then do
    --             let items = getinventory container gw
    --             msg $ LookIn containerTag (oxfordEntityNames items)
    --         else msg $ NotAContainer containerTag

-- | Look at a specific item or entity
lookAt :: Text -> Text -> World -> GameStateText
lookAt eTag loc gw = undefined
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
            let surroundings = oxfordSomeEntityNames (getActorVisibleEntitiesAtLoc gw)
            msg2 (YouAreIn $ getActiveActorLocation gw) (LookAround surroundings)

        UnaryExpression _ (NounClause invClause)
            | "inventory" `isSuffixOf` invClause ->
                return $ lookInActorInventory gw
            | otherwise ->
                lookAt invClause "TBD" gw

        BinaryExpression _ (PrepClause prep) (NounClause target)
            | prep `isPrepVariantOf` "at" || prep `isPrepVariantOf` "toward" ->
                lookAt target "TBD" gw
            | prep `isPrepVariantOf` "in" ->
                lookInContainer target gw
            | otherwise ->
                return "TBD"

        ComplexExpression _ (NounClause object) (PrepClause prep) (NounClause target) ->
            return $ "You look at " <> object <> " " <> prep <> " " <> target <> "."
