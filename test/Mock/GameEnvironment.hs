module Mock.GameEnvironment where

import           Core.State
import           Test.Hspec

-- Common test locations
testCave :: Location
testCave = Location
    { locTag = "cave"
    , locName = "A dark cave"
    , destinationTags = ["meadow", "forest"]
    }

testMeadow :: Location
testMeadow = Location
    { locTag = "meadow"
    , locName = "A flowery meadow"
    , destinationTags = ["cave"]
    }

testForest :: Location
testForest = Location
    { locTag = "forest"
    , locName = "A dense forest"
    , destinationTags = ["cave"]
    }

-- Common test characters
testAlice :: Location -> Character
testAlice loc = Character
    { charTag = TaggedEntity { tag = "alice" , name = "Alice the Adventurer" }
    , currentLocation = loc
    }

testBob :: Location -> Character
testBob loc = Character
    { charTag = TaggedEntity { tag = "bob" , name = "Bob the Brave" }
    , currentLocation = loc
    }

-- World builders
makeTestWorld :: Character -> [Character] -> [Location] -> GameWorld
makeTestWorld active playable locs = GameWorld
    { activeCharacter = active
    , playableCharacters = playable
    , locations = locs
    }

-- Common world configurations
defaultGameWorld :: GameWorld
defaultGameWorld = makeTestWorld
    (testAlice testCave)
    []
    [testCave, testMeadow]

worldWithMultipleCharacters :: GameWorld
worldWithMultipleCharacters = makeTestWorld
    (testAlice testCave)
    [testBob testMeadow]
    [testCave, testMeadow, testForest]

-- Helper functions for common test operations
withCharacterAt :: GameWorld -> Location -> GameWorld
withCharacterAt world newLoc = world
    { activeCharacter = (activeCharacter world) { currentLocation = newLoc } }

withPlayableCharacters :: GameWorld -> [Character] -> GameWorld
withPlayableCharacters world chars = world { playableCharacters = chars }

withLocations :: GameWorld -> [Location] -> GameWorld
withLocations world locs = world { locations = locs }
