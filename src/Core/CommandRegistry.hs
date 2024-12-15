module Core.CommandRegistry
    ( module Core.Registry.GameCommand
    , module Core.Registry.SystemCommand
    , allCommands
    , getKnownVerbs
    ) where

import           Command.Types
import           Core.Registry.GameCommand
import           Core.Registry.SystemCommand
import           Data.Text                   hiding (concatMap)

allCommands :: CommandRegistry
allCommands =
  CommandRegistry $
    getCommands gameCommandRegistry ++ getCommands systemCommandRegistry

getKnownVerbs :: CommandRegistry -> [Text]
getKnownVerbs (CommandRegistry commands) =
  concatMap (\cmdInfo -> cmdText cmdInfo : cmdAliases cmdInfo) commands
