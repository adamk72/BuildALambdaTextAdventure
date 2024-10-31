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
    let loc = locName $ currentLocation $ activeCharacter gw
    return $ "You are in " <> loc <> ". " <>
                     "You look around carefully, taking in your surroundings."
executeLook Nothing = do
    gw <- get
    let loc = locName $ currentLocation $ activeCharacter gw
    return $ "You are in " <> loc <> "."
executeLook (Just direction) = do
    return ("You look " <> direction <> ", but see nothing special.")

isDirectionalLook :: T.Text -> Maybe T.Text
isDirectionalLook input =
    let directions = ["north", "south", "east", "west"]
    in if input `elem` directions
       then Just input
       else Nothing
