module Repl.Repl (loop) where

import           Control.Monad.State
import           Core.Config         (replPrompt)
import           Core.State          (GameWorld)
import           Data.Text           (Text)
import qualified Data.Text.IO        as TIO
import           Repl.Parse          (parse)
import           System.IO           (hFlush, stdout)

loop :: GameWorld -> IO (Maybe GameWorld)
loop gw = do
  input <- read_
  let (outM, st) = runState (eval_ input) gw
  Just st <$ mapM_ print_ outM
  -- Todo: Note this as a trigger pattern
  -- case outM of
  --   Just out -> print_ out >> return (Just st)
  --   Nothing -> return Nothing
  {-
  The operator <$ is like a replacement operator - it performs the right side action but returns the left side value (wrapped in the same context). In this case, it prints any output but returns the new state
  -}

read_ :: IO Text
read_ = TIO.putStr replPrompt >>
        hFlush stdout >>
        TIO.getLine

print_ :: Text -> IO ()
print_ = TIO.putStrLn

eval_ :: Text -> State GameWorld (Maybe Text)
eval_ input = do
  parse input