module Repl.Repl (loop) where

import           Control.Monad.State
import           Core.Config         (replPrompt)
import           Core.State          (GameWorld)
import           Data.Text           (Text)
import qualified Data.Text.IO        as TIO
import           Repl.Parse          (parse)
import           System.IO           (hFlush, stdout)

loop :: GameWorld -> IO (Maybe GameWorld)
loop p = do
  input <- read_
  let (o, s) = runState (eval_ input) p
  if o == "quit"
  then return Nothing
  else do
    print_ o
    return (Just s)

read_ :: IO Text
read_ = TIO.putStr replPrompt >>
        hFlush stdout >>
        TIO.getLine

print_ :: Text -> IO ()
print_ = TIO.putStrLn

eval_ :: Text -> State GameWorld Text
eval_ input = do
  parse input

