{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Fuse foldr/map" #-}
module Repl.Parse (parse) where
-- import           Command.Go   (executeGo)
import           Command.Go          (executeGo)
import           Command.Look        (executeLook)
import           Control.Applicative
import           Control.Monad.State
import           Core.State          (GameWorld)
import           Data.Text           (Text, isPrefixOf, strip, stripPrefix,
                                      toLower)

data Command = Command
  { cmdName    :: Text
  , cmdExecute :: Maybe Text -> State GameWorld Text
  }

commands :: [Command]
commands =
  [ Command "look" executeLook
  , Command "go" executeGo
  ]

tryCommand :: Text -> Text -> Command -> Maybe (State GameWorld Text)
tryCommand raw lower cmd = -- Todo: ask Claude about using Command{..} to replace this (which needs RecordWildCards)
  if cmdName cmd `isPrefixOf` lower
    then Just $ cmdExecute cmd (strip <$> stripPrefix (cmdName cmd) raw)
    else Nothing

parse :: Text -> State GameWorld Text
parse input = do
  let lower = toLower input
      -- Todo: Explain the foldr and <|> operator later in the blog; see below
      firstMatch = foldr (<|>) Nothing $ map (tryCommand input lower) commands
  case firstMatch of
    Just action -> action
    Nothing     -> return $ "Don't know how to " <> lower <> "."

{-
(<|>) is the alternative operator from the Alternative typeclass in Haskell. For Maybe values, it acts like an "or" operation - it returns the first Just value it finds, or Nothing if both options are Nothing.
-}
