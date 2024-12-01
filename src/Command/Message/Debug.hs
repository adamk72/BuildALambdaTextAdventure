module Command.Message.Debug (DebugMessage (..), formatWorld) where

import           Command.Message.Common  (MessageRenderer (..))
import qualified Data.Map                as Map
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Entity.Class.EntityBase (getName)
import           Entity.Entity
import           Entity.Types.Common

data DebugMessage =
    ShowWorldState Text |
    InvalidDebugCommand Text
    deriving (Eq, Show)

instance MessageRenderer DebugMessage where
    renderMessage = \case
        ShowWorldState txt -> txt
        InvalidDebugCommand cmd -> "Invalid debug command: " <> cmd

formatEntityBase :: EntityBase a -> Text
formatEntityBase base =
    "ID: " <> unEntityId (entityId base) <>
    ", Name: " <> entityName base

formatLocation :: Entity 'LocationT -> Text
formatLocation loc =
    "\n  Location: " <> formatEntityBase (locationBase loc) <>
    "\n    Destinations: " <> T.intercalate ", " (map unEntityId $ destinations loc)

formatActor :: Entity 'ActorT -> Text
formatActor actor =
    "\n  Actor: " <> formatEntityBase (actorBase actor) <>
    "\n    Location: " <> unEntityId (actorLocationId actor)

formatItem :: Entity 'ItemT -> Text
formatItem item =
    "\n  Item: " <> formatEntityBase (itemBase item) <>
    "\n    Container: " <> unEntityId (itemLocationId item) <>
    case itemCapacity item of
        Just _  -> " (is a container)"
        Nothing -> ""

formatWorld :: World -> Text
formatWorld World{..} = T.intercalate "\n" [
    "=== World State Debug View ===",
    "\nActive Actor: " <> getName activeActor,
    "\nLocations:" <> T.concat (map formatLocation $ Map.elems locations),
    "\nActors:" <> T.concat (map formatActor $ Map.elems actors),
    "\nItems:" <> T.concat (map formatItem $ Map.elems items),
    "\n=== End Debug View ===\n"
    ]
