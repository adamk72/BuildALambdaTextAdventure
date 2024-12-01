{-# LANGUAGE OverloadedStrings #-}
module Scenario.ConditionSpec (spec) where

import           Command.Commands     (executeGo)
import           Command.TestUtils    (initTestState, runCommand)
import           Core.State           (World (..))
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Entity.Class.Movable
import           Entity.Entity
import           Entity.Types         (Capacity (..))
import           Entity.Types.Common  (EntityId (..))
import           Parser.Types
    (CmdExpression (..), CondExpression (..), NounClause (..), PossessionClause (..), StateClause (..), SubjClause (..))
import           Scenario.Types
import           Test.Hspec

-- Helper to create guard-related conditions
guardAtCastleEntry :: CondExpression
guardAtCastleEntry = AtLocationExpression (SubjClause "guard") (StateClause "castle entry")

guardHasCoinExp :: CondExpression
guardHasCoinExp = PossessiveExpression (SubjClause "guard") (PossessionClause "silver coin")

guardDoesNotHaveCoinExp :: CondExpression
guardDoesNotHaveCoinExp = NonPossessiveExpression (SubjClause "guard") (PossessionClause "silver coin")

-- Helper to create base test world setup
createBaseTestWorld :: World
createBaseTestWorld = World
    { locations = Map.fromList [(EntityId "castle entry", testCastleEntry), (EntityId "castle", testCastle)]
    , actors = Map.singleton (EntityId "guard") testGuard
    , items = Map.singleton (EntityId "silver coin") silverCoin
    , activeActor = testAlice
    , scenarios = Map.empty  -- We'll add the specific scenario in each test
    }
  where
    testGuard = Actor
        { actorBase = EntityBase
            { entityId = EntityId "guard"
            , entityTags = Nothing
            , entityName = "Greg the Guard"
            }
        , actorLocationId = EntityId "castle entry"
        , actorCapacity = Limited 10
        }

    silverCoin = Item
        { itemBase = EntityBase
            { entityId = EntityId "silver coin"
            , entityTags = Nothing
            , entityName = "a silver coin"
            }
        , itemLocationId = EntityId "castle entry"  -- Coin is at castle entry, not with guard
        , itemCapacity = Nothing
        }

    testAlice = Actor
        { actorBase = EntityBase
            { entityId = EntityId "alice"
            , entityTags = Nothing
            , entityName = "Alice"
            }
        , actorLocationId = EntityId "castle entry"
        , actorCapacity = Limited 10
        }

    testCastleEntry = Location
        { locationBase = EntityBase
            { entityId = EntityId "castle entry"
            , entityTags = Nothing
            , entityName = "Castle Entry"
            }
        , destinations = [EntityId "castle"]
        , locationCapacity = Unlimited
        }

    testCastle = Location
        { locationBase = EntityBase
            { entityId = EntityId "castle"
            , entityTags = Nothing
            , entityName = "Castle"
            }
        , destinations = [EntityId "castle entry"]
        , locationCapacity = Unlimited
        }

-- Create world with "has coin" condition scenario
createHasCoinWorld :: World
createHasCoinWorld = baseWorld { scenarios = Map.singleton (EntityId "castle entry") hasCoinScenario }
  where
    baseWorld = createBaseTestWorld
    hasCoinScenario = Scenario
        { tag = EntityId "castle entry"
        , name = "Castle entry scenario - has coin check"
        , startConditions = []
        , endConditions =
            [ ConditionGroup
                { conditions = All
                    [ guardAtCastleEntry
                    , guardHasCoinExp
                    ]
                , whileFalse = Just [ScenarioResponse
                    { actions = [UnaryCmdExpression "go" (NounClause "castle")]
                    , response = "The guard puts out a hand to stop you."
                    }]
                , whileTrue = Nothing
                }
            ]
        , actionsOnceTrue = Nothing
        }

-- Create world with "does not have coin" condition scenario
createNotHaveCoinWorld :: World
createNotHaveCoinWorld = baseWorld { scenarios = Map.singleton (EntityId "castle entry") notHaveCoinScenario }
  where
    baseWorld = createBaseTestWorld
    notHaveCoinScenario = Scenario
        { tag = EntityId "castle entry"
        , name = "Castle entry scenario - does not have coin check"
        , startConditions = []
        , endConditions =
            [ ConditionGroup
                { conditions = All
                    [ guardAtCastleEntry
                    , guardDoesNotHaveCoinExp
                    ]
                , whileFalse = Nothing
                , whileTrue = Just [ScenarioResponse
                    { actions = [UnaryCmdExpression "go" (NounClause "castle")]
                    , response = "The guard puts out a hand to stop you."
                    }]
                }
            ]
        , actionsOnceTrue = Nothing
        }

spec :: Spec
spec = describe "Scenario End Conditions" $ do
    describe "when guard does not have coin" $ do
        it "should block entry due to whileFalse trigger when checking 'has coin'" $ do
            let world = createHasCoinWorld
                expr = UnaryCmdExpression "go" (NounClause "castle")
            (response, finalWorld) <- runCommand executeGo expr world
            response `shouldBe` "The guard puts out a hand to stop you."
            getLocationId (activeActor finalWorld) `shouldBe` EntityId "castle entry"

        it "should block entry due to whileTrue trigger when checking 'does not have coin'" $ do
            let world = createNotHaveCoinWorld
                expr = UnaryCmdExpression "go" (NounClause "castle")
            (response, finalWorld) <- runCommand executeGo expr world
            response `shouldBe` "The guard puts out a hand to stop you."
            getLocationId (activeActor finalWorld) `shouldBe` EntityId "castle entry"
