module Core.Config (quitCommands, replPrompt) where

import qualified Data.Text as T

quitCommands :: [T.Text]
quitCommands = [":quit", ":q", ":exit", ":e"]

replPrompt :: T.Text
replPrompt = "λ> "
