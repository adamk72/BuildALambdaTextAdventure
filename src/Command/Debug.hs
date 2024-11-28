module Command.Debug (executeDebug) where

import           Command.CommandExecutor
import           Command.Message
import           Command.Message.Debug
import           Core.GameMonad
import           Core.State
import           Data.Text               (Text)
import           Entity.Entity
import           Parser.Types

executeDebug :: CommandExecutor
executeDebug expr = do
    gw <- getWorld
    case expr of
        UnaryCmdExpression _ (NounClause cmd)
            | cmd == "world" -> msg $ ShowWorldState $ formatWorld gw
            | otherwise -> msg $ InvalidDebugCommand cmd
        _ -> msg $ InvalidDebugCommand "Debug commands must be in the form ':debug <command>'"
