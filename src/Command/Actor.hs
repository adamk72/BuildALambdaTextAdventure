module Command.Actor (module Command.Actor) where
import Command.CommandExecutor

import Command.Look
import Parser.Types (NounClause(NounClause), CmdExpression (UnaryCmdExpression))

executeInventory :: CommandExecutor
executeInventory _ = do
  runScenarioCheck executeLook (UnaryCmdExpression "look" (NounClause "inventory"))
