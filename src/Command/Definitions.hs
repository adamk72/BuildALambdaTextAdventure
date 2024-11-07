{-# LANGUAGE OverloadedStrings #-}

module Command.Definitions
    ( CommandVerb(..)
    , CommandInfo(..)
    , allCommands
    , knownVerbs
    , findCommand
    ) where

import qualified Data.Text as T
import Control.Monad.State
import Core.State
import Command.Common
import Command.Actor
import Command.Drop
import Command.Get
import Command.Go
import Command.Look
import Data.List (find)
import Command.Put

-- Command verb data type to centralize all command identifiers
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

-- Command information including verb variants and executor
data CommandInfo = CommandInfo
    { cmdVerb :: CommandVerb   -- Primary verb identifier
    , cmdText :: T.Text        -- Primary text representation
    , cmdAliases :: [T.Text]   -- Alternative text representations
    , cmdExec :: CommandExecutor  -- Command execution function
    }

-- All supported commands with their variations and executors
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
