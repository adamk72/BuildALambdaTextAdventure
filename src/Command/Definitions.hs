{-# LANGUAGE OverloadedStrings #-}

module Command.Definitions
    ( CommandVerb(..)
    , CommandInfo(..)
    , allCommands
    , knownVerbs
    , findCommand
    ) where

import qualified Data.Text as T
import Command.Common
import Command.Actor
import Command.Drop
import Command.Get
import Command.Go
import Command.Look
import Data.List (find)
import Command.Put

data CommandVerb
    = LookVerb
    | GoVerb
    | GetVerb
    | DropVerb
    | PutVerb
    | PlaceVerb
    | MoveVerb
    | SetVerb
    | InventoryVerb
    deriving (Show, Eq)

data CommandInfo = CommandInfo
    { cmdVerb :: CommandVerb
    , cmdText :: T.Text
    , cmdAliases :: [T.Text]
    , cmdExec :: CommandExecutor
    }

allCommands :: [CommandInfo]
allCommands =
    [ CommandInfo LookVerb "look" [] executeLook
    , CommandInfo GoVerb "go" [] executeGo
    , CommandInfo GetVerb "get" [] executeGet
    , CommandInfo DropVerb "drop" [] executeDrop
    , CommandInfo PutVerb "put" [] executePut
    , CommandInfo PlaceVerb "place" [] executePut
    , CommandInfo MoveVerb "move" [] executePut
    , CommandInfo SetVerb "set" [] executePut
    , CommandInfo InventoryVerb "inventory" ["inv", "i"] executeInventory
    ]

knownVerbs :: [T.Text]
knownVerbs = concatMap (\cmd -> cmdText cmd : cmdAliases cmd) allCommands

findCommand :: T.Text -> Maybe CommandInfo
findCommand text = find matchingCommand allCommands
  where
    matchingCommand cmd =
        text == cmdText cmd || text `elem` cmdAliases cmd
