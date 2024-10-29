{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Core.State (Character(..), Location(..), initialWorld, GameWorld(..)) where

import qualified Data.Text as T

data GameWorld = GameWorld {
    -- metadata :: Metadata,
    -- regions :: [Region],
    -- currentRegion :: String,
    activeCharacter :: Character
} deriving (Show)

data Location = Location T.Text deriving Show
data Character = Character {location :: Location} deriving Show

initialWorld :: GameWorld
initialWorld = GameWorld $ Character (Location "Meadow")

