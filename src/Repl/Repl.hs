module Repl.Repl (loop) where

import           Control.Monad.State
import           Core.Config         (quitCommands, replPrompt)
import           Core.State          (Player)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
-- import           Repl.Parse          (parse)
import           Locations     (executeLook, isDirectionalLook)
import           System.IO           (hFlush, stdout)

loop :: Player -> IO Bool
loop p = do
  input <- read_
  if input `elem` quitCommands
     then return True
     else do
       print_ (evalState (eval_ input) p)
       return False

read_ :: IO T.Text
read_ = TIO.putStr replPrompt >>
        hFlush stdout >>
        TIO.getLine

print_ :: T.Text -> IO ()
print_ = TIO.putStrLn

eval_ :: T.Text -> State Player T.Text
eval_ input = do
  player <- get
  let lowerCase = T.toLower input
  case lowerCase of
    "look" -> do
        let text = executeLook player Nothing
        return text
    "look around" -> do
        let text = executeLook player (Just "around")
        return text
    other -> case isDirectionalLook other of
        Just direction -> do
            let text = executeLook player (Just direction)
            return text
        Nothing ->
            return $ "I don't know how to '" <> input <> "'."
