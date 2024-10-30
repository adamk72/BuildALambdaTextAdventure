module Core.Launch (launch) where

import           Control.Monad (unless)
import           Core.State    (GameWorld, GameEnvironment(world), loadGameEnvironmentJSON)
import qualified Repl.Repl     as Repl (loop)

gameLoop :: GameWorld -> IO ()
gameLoop gw = do
    quit <- Repl.loop gw
    unless quit (gameLoop gw)

{-
GameWorld {activeCharacter = Character {charTag = "alice", charName = "Alice the Adventurer", currentLocation = Location {locTag = "meadow", locName = "The Meadow"}}, playableCharacters = [Character {charTag = "bob", charName = "Bob the Burglar", currentLocation = Location {locTag = "cave", locName = "The Cave"}}], locations = [Location {locTag = "meadow", locName = "The Meadow"},Location {locTag = "cave", locName = "The Cave"}]}
-}
launch :: FilePath -> IO ()
launch fp = do
   geJSON <- loadGameEnvironmentJSON fp
   case geJSON of
    Right adventure -> do
        print $ world adventure
        gameLoop $ world adventure
    Left e -> print e
