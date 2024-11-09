{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Command.Look (executeLook, renderMessage) where

import           Command.CommandExecutor
import           Control.Monad.State
import           Core.Message
import           Core.State
import           Parser.Types
import           Utils
import Data.Text (Text)

lookIn :: Text -> GameWorld -> State GameWorld Text
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

executeLook :: CommandExecutor
executeLook expr = do
    gw <- get
    let acLoc = getActiveActorLoc gw
        objs = getItemsAtLoc acLoc gw
    case expr of
        AtomicExpression {} ->
            msg $ YouSeeGeneral "A general view of the space and possibly some items"
        UnaryExpression _ (NounClause "around") ->
            msg2 (YouAreIn $ locName acLoc) (LookAround objs)
        BinaryExpression _ (PrepClause "at") (NounClause target) ->
            return $ "You look at "  <> target <> "."
        BinaryExpression _ (PrepClause "in") (NounClause container) ->
            lookIn container gw
        ComplexExpression _ (NounClause itemTag) _ (NounClause containerTag) ->
            msg $ PENDING
