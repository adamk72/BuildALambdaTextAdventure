{-# LANGUAGE FlexibleInstances #-}
module Command.CommandDefinition (CommandDefinition (..), allCommands, findCommand, knownCmdVerbs) where

import           Command.CommandExecutor
import           Command.Commands
import           Command.Debug
import           Data.List               (find)
import qualified Data.Text               as T

data CommandDefinition = CommandDefinition
    { cmdText    :: T.Text
    , cmdAliases :: [T.Text]
    , cmdExec    :: Either CommandExecutor ScenarioCheckExecutor
    }

allCommands :: [CommandDefinition]
allCommands =
    [ CommandDefinition "look" ["search", "examine"] (Right executeLook)
    , CommandDefinition "go" [] (Right executeGo)
    , CommandDefinition "get" ["take"] (Left executeGet)
    , CommandDefinition "drop" [] (Left executeDrop)
    , CommandDefinition "put" ["place", "move", "set"] (Left executePut)
    , CommandDefinition "give" ["hand"] (Left executeGive)
    , CommandDefinition "inventory" ["inv", "i"] (Left executeInventory)
    , CommandDefinition ":debug" [":d", ":dbg"] (Left executeDebug)
    ]

knownCmdVerbs :: [T.Text]
knownCmdVerbs = concatMap (\cmd -> cmdText cmd : cmdAliases cmd) allCommands

findCommand :: T.Text -> Maybe CommandDefinition
findCommand text = find matchingCommand allCommands
  where
    matchingCommand cmd =
        text == cmdText cmd || text `elem` cmdAliases cmd
