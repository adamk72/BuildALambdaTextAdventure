{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Story.Locations (executeLook) where

import           Core.State (Player(..), Location(..))
import qualified Data.Text  as T

executeLook :: Player -> Maybe T.Text ->  T.Text
executeLook player (Just "around") =
    let Location locName = location player
        description = "You are in " <> locName <> ". " <>
                     "You look around carefully, taking in your surroundings."
    in description  -- Return same player state with description
executeLook player Nothing =
    let Location locName = location player
        description = "You are in " <> locName <> "."
    in description
executeLook _ (Just direction) =
    "You look " <> direction <> ", but see nothing special."