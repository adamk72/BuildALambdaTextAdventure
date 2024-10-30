{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Command.Look (executeLook, isDirectionalLook) where

import           Control.Monad.State
import           Core.State          (Character (..), GameWorld (..),
                                      Location (..))
import qualified Data.Text           as T

executeLook :: Maybe T.Text -> State GameWorld T.Text
executeLook (Just "around") = do
    gw <- get
    let loc = locName $ currentLocation $ activeCharacter $ gw
    return $ "You are in " <> loc <> ". " <>
                     "You look around carefully, taking in your surroundings."
executeLook Nothing = do
    gw <- get
    let loc = locName $ currentLocation $ activeCharacter $ gw
    return $ "You are in " <> loc <> "."
executeLook (Just direction) = do
    return ("You look " <> direction <> ", but see nothing special.")

isDirectionalLook :: T.Text -> Maybe T.Text
isDirectionalLook input =
    let directions = ["north", "south", "east", "west"]
        prefix = "look "
    in if T.isPrefixOf prefix input -- isPrefixOf: takes two Texts and returns True if and only if the first is a prefix of the second.
       then let direction = T.drop (T.length prefix) input
            in if direction `elem` directions
               then Just direction
               else Nothing
       else Nothing
