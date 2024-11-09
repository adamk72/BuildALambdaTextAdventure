{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Command.Look (executeLook, renderMessage) where

import           Command.Messages
import           Control.Monad.State
import           Core.State
import           Parser.Types

executeLook :: CommandExecutor
executeLook (UnaryExpression _ (NounClause "around")) = do
    gw <- get
    let acLoc = getActiveActorLoc gw
        objs = getItemsAtLoc acLoc gw
    return $ renderMessage (YouAreIn $ locName acLoc) <> " " <> renderMessage (LookAround objs)
executeLook _ = do
    gw <- get
    let loc = locName $ getActiveActorLoc gw
    return $ renderMessage $ YouAreIn loc
