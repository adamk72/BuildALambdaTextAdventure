module Command.Common (module Command.Common) where

import           Control.Monad.State
import           Core.State
import           Data.Text           (Text)

class CommandMessage a where
    renderMessage :: a -> Text

type CommandExecutor = Text -> State GameWorld Text
