{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Command.Look (executeLook, renderMessage) where

import           Command.CommandExecutor
import           Control.Monad.State
import           Core.Message
import           Core.State
import           Parser.Types

executeLook :: CommandExecutor
executeLook (UnaryExpression _ (NounClause "around")) = do
    gw <- get
    let acLoc = getActiveActorLoc gw
        objs = getItemsAtLoc acLoc gw
    msg2 (YouAreIn $ locName acLoc) (LookAround objs)
executeLook _ = do
    gw <- get
    let loc = locName $ getActiveActorLoc gw
    return $ renderMessage $ YouAreIn loc
