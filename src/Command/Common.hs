module Command.Common (module Command.Common) where

import           Control.Monad.State
import           Core.State
import           Data.Text           (Text)

class CommandMessage a where
    renderMessage :: a -> Text

type CommandExecutor = Maybe Text -> State GameWorld Text

-- Common utilities used across commands
getItemsAtLocation :: GameWorld -> Location -> [Item]
getItemsAtLocation gw loc =
    filter (\item -> getLocation item == loc) $ gwItems gw
