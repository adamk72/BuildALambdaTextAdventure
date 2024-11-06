{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Fuse foldr/map" #-}

module Repl.Parse (parse) where

import           Command.Actor
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
import           Data.Text           (Text, isPrefixOf, strip, stripPrefix,
                                      toLower)
import           Repl.Parser

data Command = Command
  { cmdName    :: Text
  , cmdExecute :: Text -> State GameWorld Text
  }

commands :: [Command]
commands =
  [ Command "look" executeLook
  , Command "go" executeGo
  , Command "get" executeGet
  , Command "drop" executeDrop
  , Command "put" executePut
  , Command "inventory" executeInventory
  ]

-- Refactored version
tryCommand :: Text -> Command -> Maybe (State GameWorld Text)
tryCommand input cmd = do
  -- Todo: blog post on guard: guard :: Alternative f => Bool -> f ()
  -- Only proceed if cmd name is a prefix of input
  guard $ cmdName cmd `isPrefixOf` input
  args <- getVerb <$> parseActionPhrase input
  Just $ cmdExecute cmd args

{- Old version
tryCommand :: Text -> Command -> Maybe (State GameWorld Text)
tryCommand input cmd =
  if cmdName cmd `isPrefixOf` input
    then do
      case (strip <$> stripPrefix (cmdName cmd) input) of
         Just thing -> Just $ cmdExecute cmd $ thing
         Nothing -> Nothing
    else Nothing
-}

parse :: Text -> State GameWorld (Maybe Text)
parse input = do
  let lower = toLower input
      -- Todo: Explain the foldr and <|> operator later in the blog; see below
      match = foldr (<|>) Nothing $ map (tryCommand lower) commands
  case match of
    Just action -> do
        Just <$> action
        -- Todo: Note this as a trigger pattern
        -- result <- action
        -- return $ Just result
    Nothing     -> do
      if lower `elem` quitCommands
      then return Nothing
      else return $ Just $ "Don't know how to " <> lower <> "."

{-
(<|>) is the alternative operator from the Alternative typeclass in Haskell. For Maybe values, it acts like an "or" operation - it returns the first Just value it finds, or Nothing if both options are Nothing.
-}
