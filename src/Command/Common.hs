{-# LANGUAGE LambdaCase #-}
module Command.Common (module Command.Common) where

import           Control.Monad.State
import           Core.State
import           Data.Text
import Utils

type CommandExecutor = Text -> State GameWorld Text

class CommandMessage a where
    renderMessage :: a -> Text

data CommandMessageType
    = PickedUp Text Text
    | InvalidItem Text
    | LocationDoesNotExist Text
    | AlreadyAtLocation Text
    | MovingToLocation Text
    | ItemDoesNotExist Text
    | NoLocationSpecified
    | NoPath Text
    | LookAround [Item]
    | YouAreIn Text
    | PutItemIn Text Text
    | DontKnowWhere Text
    | NotAContainer Text
    | LookTowards Text
    | DoNotSeeItem Text
    deriving (Eq, Show)

instance CommandMessage CommandMessageType where
    renderMessage = \case
        -- non-existence
        LocationDoesNotExist loc -> "Location does not exist in this game world: " <> loc <> "."
        ItemDoesNotExist item -> "Item does not exist in this game world: " <> item <> "."
        DoNotSeeItem item -> "Don't see a " <> item <> "."
        NoPath loc -> "There is no indication there's a way to get to \"" <> loc <> "\"."
        NoLocationSpecified -> "Unable to find a location at all."
        DontKnowWhere item -> "Don't know where to put " <> item <> "."
        NotAContainer item -> "The " <> item <> " is not a container."
        -- other
        PickedUp item actor -> "Moved " <> item <> " to " <> actor
        InvalidItem item -> "Cannot pick up \"" <> item <> "\"."
        AlreadyAtLocation loc -> "You're already in " <> loc <> "."
        MovingToLocation loc -> "Moving to " <> loc <> "."
        LookAround objs -> "You look around and see " <> oxfordEntityNames objs <> "."
        YouAreIn loc ->  "You are in " <> toLower loc <> "."
        PutItemIn item dst -> item <> " is now in the " <> dst <> "."
        LookTowards dir -> "You look " <> toLower dir <> ", but see nothing special."

