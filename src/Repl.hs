module Repl (loop) where

import           Config       (quitCommands, replPrompt)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           Parse        (parse)
import           System.IO    (hFlush, stdout)

loop :: IO Bool
loop = do
  input <- read_
  if input `elem` quitCommands
     then return True
     else do
       print_ (eval_ input)
       return False

read_ :: IO T.Text
read_ = TIO.putStr replPrompt >>
        hFlush stdout >>
        TIO.getLine

eval_ :: T.Text -> T.Text
eval_ = parse

print_ :: T.Text -> IO ()
print_ = TIO.putStrLn
