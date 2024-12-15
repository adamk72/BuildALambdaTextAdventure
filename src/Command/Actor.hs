module Command.Actor (executeInventory) where

import           Command.Executor
import           Command.Look
import           Parser.Types     (CmdExpression (UnaryCmdExpression), NounClause (NounClause))

executeInventory :: BasicCommandExecutor
executeInventory _ = do
  runScenarioCheck executeLook (UnaryCmdExpression "look" (NounClause "inventory"))
