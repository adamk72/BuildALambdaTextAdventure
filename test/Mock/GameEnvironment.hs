module Mock.GameEnvironment (module Mock.GameEnvironment) where

import           Core.State
import           Data.Maybe
import           Test.Hspec ()

-- Common test gwLocations
testCave ::Entity 'LocationT
testCave =Entity 'LocationT
    { locTag = "cave"
    , locName = "A dark cave"
    , destinationTags = ["meadow", "forest"]
    }

testMeadow ::Entity 'LocationT
testMeadow =Entity 'LocationT
    { locTag = "meadow"
    , locName = "A flowery meadow"
    , destinationTags = ["cave", "narnia"]
    }

testForest ::Entity 'LocationT
testForest =Entity 'LocationT
    { locTag = "forest"
    , locName = "A dense forest"
    , destinationTags = ["cave"]
    }

-- Common test characters
testAlice ::Entity 'LocationT ->Entity 'ActorT
testAlice loc = mkActor TaggedEntity
    { tag = "alice"
    , name = "Alice the Adventurer"
    , location = loc
    , inventory = JustEntity 'LocationT { locTag = "alice", locName = "your pockets", destinationTags = [] }
    }

testBob ::Entity 'LocationT ->Entity 'ActorT
testBob loc = mkActor TaggedEntity
    { tag = "bob"
    , name = "Bob the Brave"
    , location = loc
    , inventory = JustEntity 'LocationT { locTag = "bob", locName = "your pockets", destinationTags = [] }
    }

testBagOfHolding :: Item
testBagOfHolding = mkItem TaggedEntity
    { tag = "bag of holding"
    , name = "a bag of holding"
    , location = testMeadow
    , inventory = JustEntity 'LocationT { locTag = "bag of holding", locName = "inside bag", destinationTags = [] }
    }

testPearl :: Item
testPearl = mkItem TaggedEntity
    { tag = "pearl"
    , name = "a pearl of unique luster"
    , location = fromJust $ getInventory testBagOfHolding
    , inventory = Nothing
    }

testAnotherPearl :: Item
testAnotherPearl = mkItem TaggedEntity
    { tag = "another pearl"
    , name = "another pearl of unique luster"
    , location = fromJust $ getInventory testBagOfHolding
    , inventory = Nothing
    }

testBag :: Item
testBag = mkItem TaggedEntity
    { tag = "bag"
    , name = "a simple bag"
    , location = testMeadow
    , inventory = JustEntity 'LocationT { locTag = "bag", locName = "inside bag", destinationTags = [] }
    }

testBauble :: Item
testBauble = mkItem TaggedEntity
    { tag = "bauble"
    , name = "A shiny bauble"
    , location = testMeadow
    , inventory = Nothing
    }

testItemsForDefaultGw :: Entity 'ItemT
testItemsForDefaultGw =
    [ testCoin, testEightBall, testBat, testBagOfHolding, testBag, testBauble, testPearl, testAnotherPearl ]

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
makeTestWorld ::Entity 'ActorT -> Entity 'ActorT] -> Entity 'LocationT] -> Entity 'ItemT -> World
makeTestWorld active playable locs inters = World
    { gwActiveActor = active
    , gwPlayableActors = playable
    , gwLocations = locs
    , gwItems = inters
    }

-- Common world configurations
defaultGW :: World
defaultGW = makeTestWorld
    (testAlice testMeadow)
    [testBob testMeadow]
    [testCave, testMeadow, testForest]
    testItemsForDefaultGw

-- Helper functions for common test operations
withActorAt :: World ->Entity 'LocationT -> World
withActorAt w newLoc = w
    { gwActiveActor = setActorLoc newLoc (gwActiveActor w) }

withLocations :: World -> Entity 'LocationT] -> World
withLocations w locs = w { gwLocations = locs }

-- withPlayableActors :: World -> Entity 'ActorT] -> World
-- withPlayableActors world actors = world { gwPlayableActors = actors }
