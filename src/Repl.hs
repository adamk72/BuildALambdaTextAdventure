{-# LANGUAGE OverloadedStrings #-}
module Repl (loop) where

import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO
import           EvaluatorExamples
import           System.IO         (hFlush, stdout)

loop :: IO Bool
loop = do
  input <- read_
  if input == ":quit"
     then return True
     else do
       print_ (eval_ input)
       return False

read_ :: IO T.Text
read_ = TIO.putStr "REPL> " >>
        hFlush stdout >>
        TIO.getLine

eval_ :: T.Text -> T.Text
eval_ input = T.pack (emojiFinder (T.unpack input))

print_ :: T.Text -> IO ()
print_ = TIO.putStrLn
