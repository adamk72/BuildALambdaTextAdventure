{-# LANGUAGE DataKinds #-}
module Mock.TestWorld (module Mock.TestWorld) where

import qualified Data.Map                as Map
import           Entity.Class.EntityBase
import           Entity.Entity
import           Entity.Types            (Capacity (..))
import           Entity.Types.Common

-- Common test locations
testCave :: Entity 'LocationT
testCave = Location
    { locationBase = EntityBase
        { entityId = EntityId "cave"
        , entityTags = Nothing
        , entityName = "A dark cave"
        }
    , destinations = [EntityId "meadow", EntityId "forest"]
    , locationCapacity = Unlimited
    }

testMeadow :: Entity 'LocationT
testMeadow = Location
    { locationBase = EntityBase
        { entityId = EntityId "meadow"
        , entityTags = Nothing
        , entityName = "A flowery meadow"
        }
    , destinations = [EntityId "cave", EntityId "narnia"]
    , locationCapacity = Unlimited
    }

testForest :: Entity 'LocationT
testForest = Location
    { locationBase = EntityBase
        { entityId = EntityId "forest"
        , entityTags = Nothing
        , entityName = "A dense forest"
        }
    , destinations = [EntityId "cave"]
    , locationCapacity = Unlimited
    }

-- Common test actors
testAlice :: Entity 'ActorT
testAlice = Actor
    { actorBase = EntityBase
        { entityId = EntityId "alice"
        , entityTags = Nothing
        , entityName = "Alice the Adventurer"
        }
    , actorLocationId = EntityId "meadow"
    , actorCapacity = Limited 10
    }

testBob :: Entity 'ActorT
testBob = Actor
    { actorBase = EntityBase
        { entityId = EntityId "bob"
        , entityTags = Nothing
        , entityName = "Bob the Brave"
        }
    , actorLocationId = EntityId "meadow"
    , actorCapacity = Limited 10
    }

-- Common test items
testBagOfHolding :: Entity 'ItemT
testBagOfHolding = Item
    { itemBase = EntityBase
        { entityId = EntityId "bag of holding"
        , entityTags = Nothing
        , entityName = "a bag of holding"
        }
    , itemLocationId = EntityId "meadow"
    , itemCapacity = Just (Limited 20)
    }

testPearl :: Entity 'ItemT
testPearl = Item
    { itemBase = EntityBase
        { entityId = EntityId "pearl"
        , entityTags = Nothing
        , entityName = "a pearl of unique luster"
        }
    , itemLocationId = EntityId "bag of holding"
    , itemCapacity = Nothing
    }

testAnotherPearl :: Entity 'ItemT
testAnotherPearl = Item
    { itemBase = EntityBase
        { entityId = EntityId "another pearl"
        , entityTags = Nothing
        , entityName = "another pearl of unique luster"
        }
    , itemLocationId = EntityId "bag of holding"
    , itemCapacity = Nothing
    }

testBag :: Entity 'ItemT
testBag = Item
    { itemBase = EntityBase
        { entityId = EntityId "bag"
        , entityTags = Nothing
        , entityName = "a simple bag"
        }
    , itemLocationId = EntityId "meadow"
    , itemCapacity = Just (Limited 5)
    }

testBauble :: Entity 'ItemT
testBauble = Item
    { itemBase = EntityBase
        { entityId = EntityId "bauble"
        , entityTags = Nothing
        , entityName = "a shiny bauble"
        }
    , itemLocationId = EntityId "meadow"
    , itemCapacity = Nothing
    }

testCoin :: Entity 'ItemT
testCoin = Item
    { itemBase = EntityBase
        { entityId = EntityId "silver coin"
        , entityTags = Nothing
        , entityName = "a silver coin"
        }
    , itemLocationId = EntityId "meadow"
    , itemCapacity = Nothing
    }

testEightBall :: Entity 'ItemT
testEightBall = Item
    { itemBase = EntityBase
        { entityId = EntityId "eight ball"
        , entityTags = Nothing
        , entityName = "a magic eight ball"
        }
    , itemLocationId = EntityId "forest"
    , itemCapacity = Nothing
    }

testBat :: Entity 'ItemT
testBat = Item
    { itemBase = EntityBase
        { entityId = EntityId "bat"
        , entityTags = Nothing
        , entityName = "a cute bat"
        }
    , itemLocationId = EntityId "cave"
    , itemCapacity = Nothing
    }

-- World builder
defaultGW :: World
defaultGW = World
    { locations = Map.fromList [(getId loc, loc) | loc <- [testCave, testMeadow, testForest]]
    , actors = Map.fromList [(getId actor, actor) | actor <- [testAlice, testBob]]
    , items = Map.fromList [(getId item, item) | item <- allItems]
    , activeActor = testAlice
    , scenarios = Map.empty
    }
  where
    allItems = [testCoin, testEightBall, testBat, testBagOfHolding,
                testBag, testBauble, testPearl, testAnotherPearl]

-- Helper functions
withActorAt :: World -> Entity 'LocationT -> World
withActorAt world newLoc =
    world { activeActor = (activeActor world) { actorLocationId = getId newLoc } }

withLocations :: World -> [Entity 'LocationT] -> World
withLocations world newLocs =
    world { locations = Map.fromList [(getId loc, loc) | loc <- newLocs] }
