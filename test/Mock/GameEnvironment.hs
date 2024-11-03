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
testAlice loc = mkCharacter TaggedEntity
    { tag = "alice"
    , name = "Alice the Adventurer"
    , location = loc
    }

testBob :: Location -> Character
testBob loc = mkCharacter TaggedEntity
    { tag = "bob"
    , name = "Bob the Brave"
    , location = loc
    }

testInteractables :: [Interactable]
testInteractables =
    [ mkInteractable TaggedEntity
        { tag = "silver coin"
        , name = "a sliver coin"
        , location = testForest
        }
    ]

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
defaultGW = makeTestWorld
    (testAlice testMeadow)
    [testBob testMeadow]
    [testCave, testMeadow, testForest]
    testInteractables

-- Helper functions for common test operations
withCharacterAt :: GameWorld -> Location -> GameWorld
withCharacterAt w newLoc = w
    { activeCharacter = setCharLoc newLoc (activeCharacter w) }

withLocations :: GameWorld -> [Location] -> GameWorld
withLocations w locs = w { locations = locs }

-- withPlayableCharacters :: GameWorld -> [Character] -> GameWorld
-- withPlayableCharacters world chars = world { playableCharacters = chars }