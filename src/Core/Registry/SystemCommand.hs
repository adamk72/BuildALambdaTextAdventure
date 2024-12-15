module Core.Registry.SystemCommand (systemCommandRegistry) where

import           Command.Debug (executeDebug)
import           Command.Types

systemCommandRegistry :: CommandRegistry
systemCommandRegistry =
  CommandRegistry
    [ CommandInfo ":debug" [] (BasicCommand executeDebug),
      CommandInfo ":quit" [":q", ":exit", ":e"] QuitCommand
    ]
