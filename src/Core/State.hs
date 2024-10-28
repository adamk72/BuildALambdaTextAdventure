module Core.State (Player(..), Location(..), initialPlayer) where

import qualified Data.Text as T

data Location = Location T.Text deriving Show
data Player = Player {location :: Location} deriving Show

initialPlayer :: Player
initialPlayer = Player (Location "Meadow")
