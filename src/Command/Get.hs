module Command.Get where

import Data.Text
import Control.Monad.State
import Core.State.GameState

executeGet :: Maybe Text -> State GameWorld Text
executeGet target = undefined