module Mock.GameEnvironment (module Mock.GameEnvironment) where

import           Core.State
import           Data.Maybe
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
    , destinationTags = ["cave", "narnia"]
    }

testForest :: Location
testForest = Location
    { locTag = "forest"
    , locName = "A dense forest"
    , destinationTags = ["cave"]
    }

-- Common test characters
testAlice :: Location -> Actor
testAlice loc = mkActor TaggedEntity
    { tag = "alice"
    , name = "Alice the Adventurer"
    , location = loc
    , inventory = Just Location { locTag = "alice", locName = "your pockets", destinationTags = [] }
    }

testBob :: Location -> Actor
testBob loc = mkActor TaggedEntity
    { tag = "bob"
    , name = "Bob the Brave"
    , location = loc
    , inventory = Just Location { locTag = "bob", locName = "your pockets", destinationTags = [] }
    }

testBagOfHolding :: Item
testBagOfHolding = mkItem TaggedEntity
    { tag = "bag of holding"
    , name = "a bag of holding"
    , location = testMeadow
    , inventory = Just Location { locTag = "bag of holding", locName = "inside bag", destinationTags = [] }
    }

testPearl :: Item
testPearl = mkItem TaggedEntity
    { tag = "pearl"
    , name = "a pearl of unique luster"
    , location = fromJust $ getInventory testBagOfHolding
    , inventory = Nothing
    }

testBag :: Item
testBag = mkItem TaggedEntity
    { tag = "bag"
    , name = "a simple bag"
    , location = testMeadow
    , inventory = Just Location { locTag = "bag", locName = "inside bag", destinationTags = [] }
    }

testBauble :: Item
testBauble = mkItem TaggedEntity
    { tag = "bauble"
    , name = "A shiny bauble"
    , location = testMeadow
    , inventory = Nothing
    }

testItemsForDefaultGw :: [Item]
testItemsForDefaultGw =
    [ testCoin, testEightBall, testBat, testBagOfHolding, testBag, testBauble ]

testCoin :: Item
testCoin = mkItem TaggedEntity
    { tag = "silver coin"
    , name = "a silver coin"
    , location = testMeadow
    , inventory = Nothing
    }

testEightBall :: Item
testEightBall = mkItem TaggedEntity
    { tag = "eight ball"
    , name = "a magic eight ball"
    , location = testForest
    , inventory = Nothing
    }

testBat :: Item
testBat = mkItem TaggedEntity
        { tag = "bat"
        , name = "a cute bat"
        , location = testCave
        , inventory = Nothing
        }

-- World builders
makeTestWorld :: Actor -> [Actor] -> [Location] -> [Item] -> GameWorld
makeTestWorld active playable locs inters = GameWorld
    { gwActiveActor = active
    , gwPlayableActors = playable
    , gwLocations = locs
    , gwItems = inters
    }

-- Common world configurations
defaultGW :: GameWorld
defaultGW = makeTestWorld
    (testAlice testMeadow)
    [testBob testMeadow]
    [testCave, testMeadow, testForest]
    testItemsForDefaultGw

-- Helper functions for common test operations
withActorAt :: GameWorld -> Location -> GameWorld
withActorAt w newLoc = w
    { gwActiveActor = setActorLoc newLoc (gwActiveActor w) }

withLocations :: GameWorld -> [Location] -> GameWorld
withLocations w locs = w { gwLocations = locs }

-- withPlayableActors :: GameWorld -> [Actor] -> GameWorld
-- withPlayableActors world actors = world { gwPlayableActors = actors }
