{-# LANGUAGE DataKinds #-}
module Mock.TestWorld (module Mock.TestWorld) where

import           Entity.Entity
import qualified Data.Map               as Map

-- Common test locations
testCave :: Entity 'LocationT
testCave = Location
    { locationBase = EntityBase
        { entityId = EntityId "cave"
        , entityTags = Nothing
        , entityName = "A dark cave"
        }
    , destinations = [EntityId "meadow", EntityId "forest"]
    }

testMeadow :: Entity 'LocationT
testMeadow = Location
    { locationBase = EntityBase
        { entityId = EntityId "meadow"
        , entityTags = Nothing
        , entityName = "A flowery meadow"
        }
    , destinations = [EntityId "cave", EntityId "narnia"]
    }

testForest :: Entity 'LocationT
testForest = Location
    { locationBase = EntityBase
        { entityId = EntityId "forest"
        , entityTags = Nothing
        , entityName = "A dense forest"
        }
    , destinations = [EntityId "cave"]
    }

-- Common test actors
testAlice :: Entity 'ActorT
testAlice = Actor
    { actorBase = EntityBase
        { entityId = EntityId "alice"
        , entityTags = Nothing
        , entityName = "Alice the Adventurer"
        }
    , actorLocation = EntityId "meadow"
    , actorInventory = EntityBase
        { entityId = EntityId "alice-inventory"
        , entityTags = Nothing
        , entityName = "Alice's pockets"
        }
    }

testBob :: Entity 'ActorT
testBob = Actor
    { actorBase = EntityBase
        { entityId = EntityId "bob"
        , entityTags = Nothing
        , entityName = "Bob the Brave"
        }
    , actorLocation = EntityId "meadow"
    , actorInventory = EntityBase
        { entityId = EntityId "bob-inventory"
        , entityTags = Nothing
        , entityName = "Bob's pockets"
        }
    }

-- Common test items
testBagOfHolding :: Entity 'ItemT
testBagOfHolding = Item
    { itemBase = EntityBase
        { entityId = EntityId "bag of holding"
        , entityTags = Nothing
        , entityName = "a bag of holding"
        }
    , itemLocation = EntityId "meadow"
    , itemInventory = Just EntityBase
        { entityId = EntityId "bag-of-holding-inventory"
        , entityTags = Nothing
        , entityName = "inside bag of holding"
        }
    }

testPearl :: Entity 'ItemT
testPearl = Item
    { itemBase = EntityBase
        { entityId = EntityId "pearl"
        , entityTags = Nothing
        , entityName = "a pearl of unique luster"
        }
    , itemLocation = EntityId "bag-of-holding-inventory"
    , itemInventory = Nothing
    }

testAnotherPearl :: Entity 'ItemT
testAnotherPearl = Item
    { itemBase = EntityBase
        { entityId = EntityId "another pearl"
        , entityTags = Nothing
        , entityName = "another pearl of unique luster"
        }
    , itemLocation = EntityId "bag-of-holding-inventory"
    , itemInventory = Nothing
    }

testBag :: Entity 'ItemT
testBag = Item
    { itemBase = EntityBase
        { entityId = EntityId "bag"
        , entityTags = Nothing
        , entityName = "a simple bag"
        }
    , itemLocation = EntityId "meadow"
    , itemInventory = Just EntityBase
        { entityId = EntityId "bag-inventory"
        , entityTags = Nothing
        , entityName = "inside bag"
        }
    }

testBauble :: Entity 'ItemT
testBauble = Item
    { itemBase = EntityBase
        { entityId = EntityId "bauble"
        , entityTags = Nothing
        , entityName = "a shiny bauble"
        }
    , itemLocation = EntityId "meadow"
    , itemInventory = Nothing
    }

testCoin :: Entity 'ItemT
testCoin = Item
    { itemBase = EntityBase
        { entityId = EntityId "silver coin"
        , entityTags = Nothing
        , entityName = "a silver coin"
        }
    , itemLocation = EntityId "meadow"
    , itemInventory = Nothing
    }

testEightBall :: Entity 'ItemT
testEightBall = Item
    { itemBase = EntityBase
        { entityId = EntityId "eight ball"
        , entityTags = Nothing
        , entityName = "a magic eight ball"
        }
    , itemLocation = EntityId "forest"
    , itemInventory = Nothing
    }

testBat :: Entity 'ItemT
testBat = Item
    { itemBase = EntityBase
        { entityId = EntityId "bat"
        , entityTags = Nothing
        , entityName = "a cute bat"
        }
    , itemLocation = EntityId "cave"
    , itemInventory = Nothing
    }

-- World builder
defaultGW :: World
defaultGW = World
    { locations = Map.fromList [(getId loc, loc) | loc <- [testCave, testMeadow, testForest]]
    , actors = Map.fromList [(getId actor, actor) | actor <- [testAlice, testBob]]
    , items = Map.fromList [(getId item, item) | item <- allItems]
    , activeActor = testAlice
    }
  where
    allItems = [testCoin, testEightBall, testBat, testBagOfHolding,
                testBag, testBauble, testPearl, testAnotherPearl]

-- Helper functions
withActorAt :: World -> Entity 'LocationT -> World
withActorAt world newLoc =
    world { activeActor = setLocationId (getId newLoc) (activeActor world) }

withLocations :: World -> [Entity 'LocationT] -> World
withLocations world newLocs =
    world { locations = Map.fromList [(getId loc, loc) | loc <- newLocs] }