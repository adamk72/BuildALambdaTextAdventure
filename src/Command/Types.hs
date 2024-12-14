module Command.Types
    ( CommandHandler (..)
    , CommandInfo (..)
    , CommandRegistry (..)
    , CommandResult (..)
    ) where

import           Command.Executor
import           Data.Text        (Text)

data CommandResult
    = Continue Text
    | Quit
    deriving (Show, Eq)

data CommandHandler
    = BasicCommand BasicCommandExecutor
    | ScenarioCommand ScenarioCheckExecutor
    | QuitCommand


data CommandInfo = CommandInfo
    { cmdText    :: Text
    , cmdAliases :: [Text]
    , cmdHandler :: CommandHandler
    }

newtype CommandRegistry = CommandRegistry
    { getCommands :: [CommandInfo]
    }


