module Mock.GameEnvironment  (module Mock.GameEnvironment) where

import           Core.State
import           Test.Hspec ()

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

testInteractables :: [Interactable]
testInteractables = [ Interactable { interTag = TaggedEntity  {tag = "silver coin", name = "a sliver coin"} }]

-- World builders
makeTestWorld :: Character -> [Character] -> [Location] -> [Interactable] -> GameWorld
makeTestWorld active playable locs inters = GameWorld
    { activeCharacter = active
    , playableCharacters = playable
    , locations = locs
    , interactables = inters
    }

-- Common world configurations
defaultGW :: GameWorld
defaultGW = makeTestWorld (testAlice testMeadow) [testBob testMeadow] [testCave, testMeadow, testForest] testInteractables

-- Helper functions for common test operations
withCharacterAt :: GameWorld -> Location -> GameWorld
withCharacterAt w newLoc =w
    { activeCharacter = (activeCharacter w) { currentLocation = newLoc } }

withLocations :: GameWorld -> [Location] -> GameWorld
withLocations w locs = w { locations = locs }

-- withPlayableCharacters :: GameWorld -> [Character] -> GameWorld
-- withPlayableCharacters world chars = world { playableCharacters = chars }
