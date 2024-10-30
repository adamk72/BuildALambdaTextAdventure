module Repl.Repl (loop) where

import           Control.Monad.State
import           Core.Config         (quitCommands, replPrompt)
import           Core.State          (GameWorld)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           Command.Look        (executeLook, isDirectionalLook)
import            Command.Go
import           System.IO           (hFlush, stdout)

loop :: GameWorld -> IO Bool
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

eval_ :: T.Text -> State GameWorld T.Text
eval_ input = do
  let lowerCase = T.toLower input
  case lowerCase of
    "go cave" -> executeGo "cave"
    "go meadow" -> executeGo "meadow"
    "look" -> executeLook Nothing
    "look around" -> executeLook (Just "around")
    other -> case isDirectionalLook other of
        Just direction -> executeLook (Just direction)
        Nothing        -> return $ "I don't know how to '" <> input <> "'."
