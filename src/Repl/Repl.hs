module Repl.Repl (loop) where

import           Control.Monad.State
import           Core.Config         (replPrompt)
import           Core.State          (GameWorld)
import           Data.Text           (Text)
import qualified Data.Text.IO        as TIO
import           Repl.Interpreter    (interpretCommand)
import           System.IO           (hFlush, stdout)

loop :: GameWorld -> IO (Maybe GameWorld)
loop gw = do
  input <- read_
  let (outM, st) = runState (eval_ input) gw
  maybe (return Nothing) (\out -> print_ out >> return (Just st)) outM


read_ :: IO Text
read_ = TIO.putStr replPrompt >>
        hFlush stdout >>
        TIO.getLine

print_ :: Text -> IO ()
print_ = TIO.putStrLn

eval_ :: Text -> State GameWorld (Maybe Text)
eval_ input = do
  interpretCommand input
