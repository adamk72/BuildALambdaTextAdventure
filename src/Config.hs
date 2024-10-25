module Config (quitCommands, replPrompt) where

import qualified Data.Text as T

-- Define the quit command here
quitCommands :: [T.Text]
quitCommands = [":quit", ":q"]

replPrompt :: T.Text
replPrompt = "REPL> "
