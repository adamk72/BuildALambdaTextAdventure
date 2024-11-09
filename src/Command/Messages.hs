{-# LANGUAGE LambdaCase #-}
module Command.Messages (module Command.Messages) where

import           Control.Monad.State
import           Core.State
import           Data.Text
import           Parser.Types
import           Utils

type CommandExecutor = Expression -> State GameWorld Text

class CommandMessage a where
    renderMessage :: a -> Text

data CommandMessageType
    = PickedUp Text Text
    | InvalidItem Text
    | NoPath Text
    | AlreadyAtLocation Text
    | MovingToLocation Text
    | ItemDoesNotExist Text
    | NoLocationSpecified
    | LookAround [Item]
    | YouAreIn Text
    | PutItemIn Text Text
    | DontKnowWhere Text
    | NotAContainer Text
    | LookTowards Text
    | GoWhere
    | DoNotSeeItem Text
    | NotSure
    | LocationError Text
    | PENDING
    deriving (Eq, Show)

instance CommandMessage CommandMessageType where
    renderMessage = \case
        -- non-existence
        PENDING -> "Pending, not sure what to do with this yet."
        ItemDoesNotExist item -> "Item does not exist in this game world: " <> item <> "."
        DoNotSeeItem item -> "Don't see a " <> item <> "."
        NoLocationSpecified -> "Unable to find a location at all."
        DontKnowWhere item -> "Don't know where to put " <> item <> "."
        NotAContainer item -> "The " <> item <> " is not a container."
        NotSure -> "Not sure how to do that."
        -- other
        PickedUp item actor -> "Moved " <> item <> " to " <> actor
        InvalidItem item -> "Cannot pick up \"" <> item <> "\"."
        MovingToLocation loc -> "Moving to " <> loc <> "."
        LookAround objs -> "You look around and see " <> oxfordEntityNames objs <> "."
        YouAreIn loc ->  "You are in " <> toLower loc <> "."
        PutItemIn item dst -> item <> " is now in the " <> dst <> "."
        LookTowards dir -> "You look " <> toLower dir <> ", but see nothing special."
        -- Go specific
        GoWhere -> "Where do you want to go?"
        NoPath loc -> "There is no indication there's a way to get to \"" <> loc <> "\"."
        AlreadyAtLocation loc -> "You're already in " <> loc <> "."
        LocationError loc -> "ERROR: Location " <> loc <> " is not in the game world."

