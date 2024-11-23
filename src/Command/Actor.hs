module Command.Actor (module Command.Actor) where
import Command.CommandExecutor

import Command.Look
import Parser.Types (NounClause(NounClause), Expression (UnaryExpression))

executeInventory :: CommandExecutor
executeInventory _ = do
  executeLook (UnaryExpression "look" (NounClause "inventory"))
