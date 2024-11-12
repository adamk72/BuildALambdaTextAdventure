{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Command.Look (executeLook, renderMessage) where

import           Command.CommandExecutor
import           Core.Message
import           Core.State
import           Core.State.EntityContainer
import           Data.Text                  (Text, isSuffixOf, toLower)
import           Parser.Types
import           Parser.Utils
import           Utils

-- | Look inside a container
lookInContainer :: Text -> World -> GameStateText
lookInContainer containerTag gw =
    case findItemByTag containerTag gw of
        Nothing -> msg $ ItemDoesNotExist containerTag
        Just container ->
            if isContainer container
            then do
                let items = getContainerContents container gw
                msg $ LookIn containerTag (oxfordEntityNames items)
            else msg $ NotAContainer containerTag

-- | Look at a specific item or entity
lookAt :: Text -> Location -> World -> GameStateText
lookAt eTag loc gw = do
    case findEntityByTagAtLoc eTag loc gw of
        Just entity -> return $ "You see " <> getName entity <> "."
        Nothing -> do
            -- Check inventory
            case findItemByTagInActorInventory eTag gw of
                Just item -> return $ "In your inventory, you see " <> toLower (getName item) <> "."
                Nothing -> do
                    -- Check visible containers
                    let visibleContainers = filter isContainer $
                            map snd $ findAllInstances eTag gw
                    case visibleContainers of
                        (container:_) ->
                            let items = getContainerContents container gw
                            in msg $ LookIn (getTag container) (oxfordEntityNames items)
                        [] -> return $ "Don't see a " <> eTag <> " to look at."

-- | Look at inventory contents
lookInActorInventory :: World -> Text
lookInActorInventory gw =
    "Your inventory has: " <> oxfordEntityNames (getActorInventoryItems gw)

-- | Helper to find all instances of an entity by tag
findAllInstances :: Text -> World -> [(Location, Entity)]
findAllInstances targetTag gw =
    [ (loc, e)
    | e <- getEntities gw
    , getTag e == targetTag
    , let loc = getLocation e
    ]

executeLook :: CommandExecutor
executeLook expr = do
    gw <- getWorld
    let acLoc = getActiveActorLoc gw
        objs = getEntitiesAtLoc acLoc gw
    case expr of
        AtomicExpression {} ->
            msg $ YouSeeGeneral "A general view of the space and possibly some items"

        UnaryExpression _ (NounClause "around") ->
            msg2 (YouAreIn $ locName acLoc) (LookAround objs)

        UnaryExpression _ (NounClause invClause)
            | "inventory" `isSuffixOf` invClause ->
                return $ lookInActorInventory gw
            | otherwise ->
                lookAt invClause acLoc gw

        BinaryExpression _ (PrepClause prep) (NounClause target)
            | prep `isPrepVariantOf` "at" || prep `isPrepVariantOf` "toward" ->
                lookAt target acLoc gw
            | prep `isPrepVariantOf` "in" ->
                lookInContainer target gw
            | otherwise ->
                return "TBD"

        ComplexExpression _ (NounClause object) (PrepClause prep) (NounClause target) ->
            return $ "You look at " <> object <> " " <> prep <> " " <> target <> "."
