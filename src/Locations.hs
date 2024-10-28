{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Locations (executeLook, isDirectionalLook) where

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

-- Helper function to handle directional looking
isDirectionalLook :: T.Text -> Maybe T.Text
isDirectionalLook input =
    let directions = ["north", "south", "east", "west"]
        prefix = "look "
    -- isPrefixOf: takes two Texts and returns True if and only if the first is a prefix of the second.
    in if T.isPrefixOf prefix input
       then let direction = T.drop (T.length prefix) input
            in if direction `elem` directions
               then Just direction
               else Nothing
       else Nothing