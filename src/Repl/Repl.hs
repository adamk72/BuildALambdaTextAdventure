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
  maybe (return Nothing) (\out -> print_ out >> return (Just st)) outM
{- This is WAY better
  maybe :: b -> (a -> b) -> Maybe a -> b
The maybe function takes a default value, a function, and a Maybe value. If the Maybe value is Nothing, the function returns the default value. Otherwise, it applies the function to the value inside the Just and returns the result.
-}
{- Previous code for comparison:
  case outM of
    Just out -> do
        print_ out
        return (Just st)
    Nothing -> return Nothing
-}
-- Just st <$ mapM_ print_ outM -- why doesn't this work?

read_ :: IO Text
read_ = TIO.putStr replPrompt >>
        hFlush stdout >>
        TIO.getLine

print_ :: Text -> IO ()
print_ = TIO.putStrLn

eval_ :: Text -> State GameWorld (Maybe Text)
eval_ input = do
  parse input