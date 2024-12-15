module Core.Registry.GameCommand (CommandInfo (..), CommandRegistry (..), findCommand, gameCommandRegistry) where

import           Command.Commands
import           Command.Types
import           Data.List        (find)
import           Data.Text        (Text)

gameCommandRegistry :: CommandRegistry
gameCommandRegistry =
  CommandRegistry
    [ CommandInfo "look" ["search", "examine"] (ScenarioCommand executeLook),
      CommandInfo "go" [] (ScenarioCommand executeGo),
      CommandInfo "get" ["take"] (BasicCommand executeGet),
      CommandInfo "drop" [] (BasicCommand executeDrop),
      CommandInfo "put" ["place", "move", "set"] (BasicCommand executePut),
      CommandInfo "give" ["hand"] (BasicCommand executeGive),
      CommandInfo "inventory" ["inv", "i"] (BasicCommand executeInventory)
    ]

findCommand :: Text -> CommandRegistry -> Maybe CommandInfo
findCommand text (CommandRegistry commands) =
  find (\cmd -> text == cmdText cmd || text `elem` cmdAliases cmd) commands
