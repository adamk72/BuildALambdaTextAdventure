{-# LANGUAGE FlexibleInstances #-}
module Command.CommandHandler (CommandHandler (..), allCommands, findCommand, knownCmdVerbs) where

import           Command.CommandExecutor
import           Command.Commands
import           Command.Debug
import           Data.List               (find)
import qualified Data.Text               as T

data CommandHandler = CommandHandler
    { cmdText    :: T.Text
    , cmdAliases :: [T.Text]
    , cmdExec    :: Either CommandExecutor ScenarioCheckExecutor
    }

allCommands :: [CommandHandler]
allCommands =
    [ CommandHandler "look" ["search", "examine"] (Right executeLook)
    , CommandHandler "go" [] (Right executeGo)
    , CommandHandler "get" ["take"] (Left executeGet)
    , CommandHandler "drop" [] (Left executeDrop)
    , CommandHandler "put" ["place", "move", "set"] (Left executePut)
    , CommandHandler "give" ["hand"] (Left executeGive)
    , CommandHandler "inventory" ["inv", "i"] (Left executeInventory)
    , CommandHandler ":debug" [] (Left executeDebug)
    ]

knownCmdVerbs :: [T.Text]
knownCmdVerbs = concatMap (\cmd -> cmdText cmd : cmdAliases cmd) allCommands

findCommand :: T.Text -> Maybe CommandHandler
findCommand text = find matchingCommand allCommands
  where
    matchingCommand cmd =
        text == cmdText cmd || text `elem` cmdAliases cmd
