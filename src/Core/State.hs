{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveGeneric #-}
module Core.State (Character(..), Location(..), initialWorld, GameEnvironment(..), GameWorld(..)) where

import           Data.Text    as T
import           GHC.Generics (Generic)

data Metadata = Metadata {
    title       :: Text,
    launchTitle :: Text,
    description :: Text,
    version     :: Text,
    author      ::Text
} deriving (Show, Generic)

data Character = Character {
  tag :: Text,
  name :: Text,
  currentLocation :: Location
} deriving (Show)

data Location = Location {
  ltag :: Text,
  lname :: Text
} deriving (Show)

data GameWorld = GameWorld {
  activeCharacter :: Character
  -- playableCharacters :: [Character],
  -- locations :: [Location]
} deriving (Show)

data GameEnvironment = GameEnvironment {
    -- metadata :: Metadata
    world :: GameWorld
    -- currentRegion :: String,
} deriving (Show)

initialWorld :: GameEnvironment
initialWorld = GameEnvironment $ GameWorld $ Character "alice" "Alice" (Location "meadow" "The Meadow")

