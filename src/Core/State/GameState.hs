{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Core.State.GameState (AppState (..), GameEnvironment (..), GameWorld (..), Metadata (..), GameState(..), GameMonad, GameStateText) where

import           Control.Monad.State
import           Entity.Entity
import           Core.State.Location (Location)
import           Data.Aeson          (FromJSON)
import           Data.Text           (Text)
import           GHC.Generics        (Generic)
import           Logger              (GameHistory)

type GameMonad a = StateT GameState IO a

-- | Combined state for game operations
data GameState = GameState
    { gsWorld :: GameWorld
    , gsHistory :: GameHistory
    }

type GameStateText = GameMonad Text

data AppState = AppState
    { gameWorld   :: GameWorld
    , gameHistory :: GameHistory
    }

-- Todo: Refactor to to use Control.Lens
data GameWorld = GameWorld {
    gwActiveActor    :: Entity 'ActorT,
    gwPlayableActors :: [Entity 'ActorT],
    gwLocations      :: [Entity 'LocationT],
    gwItems          :: [Entity 'ItemT]
} deriving (Show, Eq, Generic)

-- Note: FromJSON instance will be defined in JSON.hs

data Metadata = Metadata {
    title       :: Text,
    launchTag   :: Text,
    description :: Text,
    version     :: Text,
    author      :: Text
} deriving (Show, Eq, Generic, FromJSON)

data GameEnvironment = GameEnvironment {
    metadata :: Metadata,
    world    :: Maybe GameWorld
} deriving (Show, Eq, Generic)

-- Note: FromJSON instance will be defined in JSON.hs
