{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Fuse foldr/map" #-}

module Repl.Parse (parse, tryCommand, Command(Command)) where

import           Command.Actor
import           Command.Common
import           Command.Definitions
import           Command.Drop
import           Command.Get
import           Command.Go
import           Command.Look
import           Command.Put
import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Core.Config         (quitCommands)
import           Core.State          (GameWorld)
import           Data.Text           (Text, isPrefixOf, toLower)
import           Repl.Parser

firstRight :: Either Text a -> Either Text a -> Either Text a
firstRight (Right x) _        = Right x
firstRight (Left _) (Right y) = Right y
firstRight (Left x) (Left _)  = Left x  -- Keep the first error we found

data Command = Command
  { cmdName    :: Text
  , cmdExecute :: CommandExecutor
  }

toCommand :: CommandInfo -> Command
toCommand info = Command (cmdText info) (cmdExec info)

commands :: [Command]
commands = map toCommand allCommands

tryCommand :: Text -> Command -> Maybe (State GameWorld Text)
tryCommand input cmd = do
  guard $ cmdName cmd `isPrefixOf` input
  -- rest <- getRest <$> parseActionPhrase input
  Just $ cmdExecute cmd input

parse :: Text -> State GameWorld (Maybe Text)
parse input = do
  let lower = toLower input
      match = foldr (<|>) Nothing $ map (tryCommand lower) commands
  case match of
    Just action -> do
        Just <$> action
    Nothing     -> do
      if lower `elem` quitCommands
      then return Nothing
      else return $ Just $ "Don't know how to " <> lower <> "."
