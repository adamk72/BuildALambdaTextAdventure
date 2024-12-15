{-# LANGUAGE QuasiQuotes #-}

module Command.Debug (executeDebug) where

import           Command.Executor
import           Command.Message
import           Command.Message.Debug
import           Core.GameMonad
import           Parser.Types
import           Text.RawString.QQ

executeDebug :: BasicCommandExecutor
executeDebug expr = do
  gw <- getWorld
  case expr of
    UnaryCmdExpression _ (NounClause "world") -> msg $ ShowWorldState $ formatWorld gw
    _ ->
      msg $
        InvalidDebugCommand
          [r|Valid debug commands are \":debug\" (or \":dbg\" or \":d\") followed by
    world                  -- gives current world information|]
