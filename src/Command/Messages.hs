{-# LANGUAGE LambdaCase #-}
module Command.Messages (module Command.Messages) where

import           Control.Monad.State
import           Core.State
import           Data.Text (Text, unpack, intercalate, toLower)
import           Parser.Types
import           Utils

type CommandExecutor = Expression -> State GameWorld Text

msg :: CommandMessageType -> State GameWorld Text
msg = return . renderMessage

msgs :: [CommandMessageType] -> Text -> State GameWorld Text
msgs messages separator = return $ intercalate separator (Prelude.map renderMessage messages)

msg2 :: CommandMessageType -> CommandMessageType -> State GameWorld Text
msg2 m1 m2 = msgs [m1, m2] " "

msgGameWordError :: CommandMessageType -> State GameWorld Text
msgGameWordError msgE = error $ unpack $ renderMessage $ msgE

class CommandMessage a where
    renderMessage :: a -> Text

data CommandMessageType
    =
     AlreadyAtLocation Text
    | DoNotSeeItem Text
    | DontKnowWhere Text
    | DropWhat
    | DroppedItem Text
    | DroppedItemSomewhere Text Text
    | DroppedItemWithInventory Text Text
    | GetWhat
    | GoWhere
    | InvalidItem Text
    | InvalidItemInContainer Text Text
    | InvalidItemInLocation Text
    | ItemDoesNotExist Text
    | ItemError Text
    | LocationError Text
    | LookAround [Item]
    | LookTowards Text
    | MovingToLocation Text
    | NoItem Text
    | NoLocationSpecified
    | NoPath Text
    | NoItemForContainer Text Text
    | NoContainerForItem Text Text
    | NotAContainer Text
    | NotSure
    | PENDING
    | PickedUp Text Text
    | PutItemIn Text Text
    | PutWhat
    | PutWhere Text
    | YouAreIn Text
    | YouDoNotHave Text
    deriving (Eq, Show)

instance CommandMessage CommandMessageType where
    renderMessage = \case
        -- non-existence
        PENDING -> "Pending, not sure what to do with this yet."
        ItemDoesNotExist item -> "Item does not exist in this game world: " <> item <> "."
        DoNotSeeItem item -> "Don't see a " <> item <> "."
        NoLocationSpecified -> "Unable to find a location at all."
        NotAContainer item -> "The " <> item <> " is not a container."
        NotSure -> "Not sure how to do that."
        -- other
        PickedUp item actor -> "Moved " <> item <> " to " <> actor
        MovingToLocation loc -> "Moving to " <> loc <> "."
        LookAround objs -> "You look around and see " <> oxfordEntityNames objs <> "."
        YouAreIn loc ->  "You are in " <> toLower loc <> "."
        PutItemIn item dst -> item <> " is now in the " <> dst <> "."
        LookTowards dir -> "You look " <> toLower dir <> ", but see nothing special."
        InvalidItem item -> "Don't see a \"" <> item <> "\"."
        -- Put Specific
        InvalidItemInLocation item -> "Don't see a \"" <> item <> "\" around here."
        InvalidItemInContainer item container -> "Don't see a \"" <> item <> "\" to put into " <> container <>"."
        PutWhat -> "What needs to be put?"
        PutWhere item -> "Put " <> item <> " where?"
        DontKnowWhere item -> "Don't know where to put " <> item <> "."
        NoItemForContainer item container -> "Don't see " <> item <> " to put in " <> container <> "."
        NoContainerForItem item container -> "Dont' have " <> container <> " to put " <> item <> "."
        -- Get Specific
        GetWhat -> "What are you trying to get?"
        -- Drop specific
        DropWhat -> "What needs to dropped?"
        DroppedItem object -> "You dropped " <> object <> "."
        DroppedItemSomewhere object loc -> "You dropped " <> object <> " " <> loc <> "."
        DroppedItemWithInventory object inv -> "You dropped " <> object <> ". Your inventory is now: " <> inv <> "."
        YouDoNotHave object -> "You don't have a " <> object <> " to drop."
        -- Go specific
        GoWhere -> "Where do you want to go?"
        NoPath loc -> "There is no indication there's a way to get to \"" <> loc <> "\"."
        NoItem item -> "There is no indication there's a \"" <> item <> "\" around here."
        AlreadyAtLocation loc -> "You're already in " <> loc <> "."
        -- GameWorld Errors
        LocationError loc -> "ERROR: Location \"" <> loc <> "\" is not in the game world."
        ItemError item -> "ERROR: Item \"" <> item <> "\" is not in the game world."

