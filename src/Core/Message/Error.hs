module Core.Message.Error
    ( ErrorMessage(..)
    ) where

import Data.Text (Text)
import Core.Message.Common (MessageRenderer(..))

data ErrorMessage
    = LocationError Text
    | ItemError Text
    deriving (Eq, Show)

instance MessageRenderer ErrorMessage where
    renderMessage = \case
        LocationError loc -> "ERROR: Location \"" <> loc <> "\" is not in the game world."
        ItemError item -> "ERROR: Item \"" <> item <> "\" is not in the game world."