module Command.Common (module Command.Common) where

import           Control.Monad.State
import           Core.State
import           Data.Text           (Text)

class CommandMessage a where
    renderMessage :: a -> Text

type CommandExecutor = Maybe Text -> State GameWorld Text

-- Common utilities used across commands
getInteractablesAtLocation :: GameWorld -> Location -> [Interactable]
getInteractablesAtLocation gw loc =
    filter (\inter -> getLocation inter == loc) $ gwInteractables gw
