module Command.CommandInfo (CommandInfo (..), CommandVerb (..), allCommands, findCommand, knownVerbs) where

import           Command.CommandExecutor
import           Command.Commands
import           Command.Debug
import           Data.List               (find)
import qualified Data.Text               as T

data CommandVerb
    = LookVerb
    | DebugVerb
    | DropVerb
    | GetVerb
    | GoVerb
    | InventoryVerb
    | MoveVerb
    | PlaceVerb
    | PutVerb
    | SetVerb
    deriving (Show, Eq)

data CommandInfo = CommandInfo
    { cmdVerb    :: CommandVerb
    , cmdText    :: T.Text
    , cmdAliases :: [T.Text]
    , cmdExec    :: CommandExecutor
    }

allCommands :: [CommandInfo]
allCommands =
    [ CommandInfo LookVerb "look" ["search", "examine"] executeLook
    , CommandInfo GoVerb "go" [] executeGo
    , CommandInfo GetVerb "get" ["take"] executeGet
    , CommandInfo DropVerb "drop" [] executeDrop
    , CommandInfo PutVerb "put" ["place", "move", "set"] executePut
    , CommandInfo InventoryVerb "inventory" ["inv", "i"] executeInventory
    , CommandInfo DebugVerb ":debug" [] executeDebug
    ]

knownVerbs :: [T.Text]
knownVerbs = concatMap (\cmd -> cmdText cmd : cmdAliases cmd) allCommands

findCommand :: T.Text -> Maybe CommandInfo
findCommand text = find matchingCommand allCommands
  where
    matchingCommand cmd =
        text == cmdText cmd || text `elem` cmdAliases cmd
