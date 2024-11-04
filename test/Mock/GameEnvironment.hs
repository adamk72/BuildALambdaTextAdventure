module Mock.GameEnvironment  (module Mock.GameEnvironment) where

import           Core.State
import           Test.Hspec ()

-- Common test gwLocations
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
    , inventory = Just [Location { locTag = "alice", locName = "your pockets", destinationTags = [] }]
    }

testBob :: Location -> Character
testBob loc = mkCharacter TaggedEntity
    { tag = "bob"
    , name = "Bob the Brave"
    , location = loc
    , inventory = Just [Location { locTag = "bob", locName = "your pockets", destinationTags = [] }]
    }

testItems :: [Item]
testItems =
    [
    mkItem TaggedEntity
        { tag = "silver coin"
        , name = "a sliver coin"
        , location = testMeadow
        , inventory = Nothing
        },
    mkItem TaggedEntity
        { tag = "eight ball"
        , name = "a magic eight ball"
        , location = testForest
        , inventory = Nothing
        }
    ]

-- World builders
makeTestWorld :: Character -> [Character] -> [Location] -> [Item] -> GameWorld
makeTestWorld active playable locs inters = GameWorld
    { gwActiveCharacter = active
    , gwPlayableCharacters = playable
    , gwLocations = locs
    , gwItems = inters
    }

-- Common world configurations
defaultGW :: GameWorld
defaultGW = makeTestWorld
    (testAlice testMeadow)
    [testBob testMeadow]
    [testCave, testMeadow, testForest]
    testItems

-- Helper functions for common test operations
withCharacterAt :: GameWorld -> Location -> GameWorld
withCharacterAt w newLoc = w
    { gwActiveCharacter = setCharLoc newLoc (gwActiveCharacter w) }

withLocations :: GameWorld -> [Location] -> GameWorld
withLocations w locs = w { gwLocations = locs }

-- withPlayableCharacters :: GameWorld -> [Character] -> GameWorld
-- withPlayableCharacters world chars = world { gwPlayableCharacters = chars }
