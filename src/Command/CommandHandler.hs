module Command.CommandHandler (CommandHandler (..), allCommands, findCommand, knownCmdVerbs) where

import           Command.CommandExecutor
import           Command.Commands
import           Command.Debug
import           Data.List               (find)
import qualified Data.Text               as T

data CommandHandler = CommandHandler
    { cmdText    :: T.Text
    , cmdAliases :: [T.Text]
    , cmdExec    :: CommandExecutor
    }

allCommands :: [CommandHandler]
allCommands =
    [ CommandHandler "look" ["search", "examine"] executeLook
    , CommandHandler "go" [] executeGo
    , CommandHandler "get" ["take"] executeGet
    , CommandHandler "drop" [] executeDrop
    , CommandHandler "put" ["place", "move", "set"] executePut
    , CommandHandler "give" ["hand"] executeGive
    , CommandHandler "inventory" ["inv", "i"] executeInventory
    , CommandHandler ":debug" [] executeDebug
    ]

knownCmdVerbs :: [T.Text]
knownCmdVerbs = concatMap (\cmd -> cmdText cmd : cmdAliases cmd) allCommands

findCommand :: T.Text -> Maybe CommandHandler
findCommand text = find matchingCommand allCommands
  where
    matchingCommand cmd =
        text == cmdText cmd || text `elem` cmdAliases cmd
