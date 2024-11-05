{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Command.Look (executeLook, LookMessage(..), renderMessage) where

import           Command.Common
import           Control.Monad.State
import           Core.State
import           Data.Text           as T
import           Utils

data LookMessage
    = LookAround [Item]
    | YouAreIn Text
    | LookTowards Text

instance CommandMessage LookMessage where
    renderMessage = \case
        LookAround objs -> "You look around and see " <> oxfordEntityNames objs <> "."
        YouAreIn loc ->  "You are in " <> T.toLower loc <> "."
        LookTowards dir -> "You look " <> T.toLower dir <> ", but see nothing special."

executeLook :: CommandExecutor
executeLook (Just "around") = do
    gw <- get
    let acLoc = getActiveActorLoc gw
        objs = getItemsAtLoc acLoc gw
    return $ renderMessage (YouAreIn $ locName acLoc) <> " " <> renderMessage (LookAround objs)
executeLook Nothing = do
    gw <- get
    let loc = locName $ getActiveActorLoc gw
    return $ renderMessage $ YouAreIn loc
executeLook (Just direction) =
    return $ renderMessage $ LookTowards direction
