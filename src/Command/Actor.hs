module Command.Actor (module Command.Actor) where
import Command.Executor

import Command.Look
import Parser.Types (NounClause(NounClause), CmdExpression (UnaryCmdExpression))

executeInventory :: BasicCommandExecutor
executeInventory _ = do
  runScenarioCheck executeLook (UnaryCmdExpression "look" (NounClause "inventory"))
