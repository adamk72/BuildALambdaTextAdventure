{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Command.Look (executeLook, LookMessage(..), renderMessage) where

import           Control.Monad.State
import           Core.State
import           Data.Text           as T
import           Helper

data LookMessage
    = LookAround [Interactable]
    | YouAreIn Text
    | LookTowards Text

renderMessage :: LookMessage -> Text
renderMessage = \case
    LookAround objs -> "You look around and see " <> oxfordEntityNames objs <> "."
    YouAreIn loc ->  "You are in " <> T.toLower loc <> "."
    LookTowards dir -> "You look " <> T.toLower dir <> ", but see nothing special."

executeLook :: Maybe Text -> State GameWorld Text
executeLook (Just "around") = do
    gw <- get
    let acLoc = getActiveCharLoc gw
        objs = Prelude.filter (\inter -> getLocation inter == acLoc) $ gwInteractables gw
    return $ renderMessage (YouAreIn $ locName acLoc) <> " " <> renderMessage (LookAround objs)
executeLook Nothing = do
    gw <- get
    let loc = locName $ getActiveCharLoc gw
    return $ renderMessage $ YouAreIn loc
executeLook (Just direction) = do
    return (renderMessage $ LookTowards direction)
