{-# LANGUAGE OverloadedStrings #-}
module Core.Launch (launch) where

import           Control.Monad     (unless)
import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO
import           EvaluatorExamples
import           System.IO         (hFlush, stdout)

launch :: IO ()
launch = do
  input <- read_
  unless (input == ":quit")
       $ print_ (eval_ input) >> launch

read_ :: IO T.Text
read_ = TIO.putStr "REPL> " >>
        hFlush stdout >>
        TIO.getLine

eval_ :: T.Text -> T.Text
eval_ input = T.pack (emojiFinder (T.unpack input))

print_ :: T.Text -> IO ()
print_ = TIO.putStrLn
