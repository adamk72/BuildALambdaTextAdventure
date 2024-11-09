module Core.Message.Error (ErrorMessage (..)) where

import           Core.Message.Common (MessageRenderer (..))
import           Data.Text           (Text)

data ErrorMessage
    = LocationError Text
    | ItemError Text
    deriving (Eq, Show)

instance MessageRenderer ErrorMessage where
    renderMessage = \case
        LocationError loc -> "ERROR: Location \"" <> loc <> "\" is not in the game world."
        ItemError item -> "ERROR: Item \"" <> item <> "\" is not in the game world."
