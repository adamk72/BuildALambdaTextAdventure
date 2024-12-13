{-# LANGUAGE OverloadedStrings #-}
module Scenario.ConditionSpec (spec) where

import qualified Data.Map              as Map
import           Data.Text             (words)
import           Entity.Entity
import           Entity.Types.Capacity
import           Entity.Types.Common   (EntityId (..))
import           Parser.Parser         (parseCondPhrase, renderExpressionError)
import           Parser.Types
import           Prelude               hiding (words)
import           Scenario.Check        (executeConditionCheck)
import           Test.Hspec

-- Helper to create a basic test world
createTestWorld :: Bool -> World
createTestWorld guardHasCoin = World
    { locations = Map.fromList [(EntityId "castle entry", testLoc)]
    , actors = Map.singleton (EntityId "guard") testGuard
    , items = Map.singleton (EntityId "silver coin") testCoin
    , activeActor = testAlice
    , scenarios = Map.empty
    }
  where
    testLoc = Location
        { locationBase = EntityBase
            { entityId = EntityId "castle entry"
            , entityTags = Nothing
            , entityName = "Castle Entry"
            }
        , destinations = []
        , locationCapacity = Unlimited
        }

    testGuard = Actor
        { actorBase = EntityBase
            { entityId = EntityId "guard"
            , entityTags = Nothing
            , entityName = "Guard"
            }
        , actorLocationId = EntityId "castle entry"
        , actorCapacity = Limited 10
        }

    testCoin = Item
        { itemBase = EntityBase
            { entityId = EntityId "silver coin"
            , entityTags = Nothing
            , entityName = "a silver coin"
            }
        , itemLocationId = if guardHasCoin
                          then EntityId "guard"
                          else EntityId "castle entry"
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

spec :: Spec
spec = do
    describe "Condition Parsing" $ do
        it "parses 'guard has silver coin'" $ do
            let result = parseCondPhrase "guard has silver coin"
            result `shouldBe` Right (PossessiveExpression
                                    (SubjClause "guard")
                                    (PossessionClause "silver coin"))

        it "parses 'guard does not have silver coin'" $ do
            let result = parseCondPhrase "guard does not have silver coin"
            result `shouldBe` Right (NonPossessiveExpression
                                    (SubjClause "guard")
                                    (PossessionClause "silver coin"))

        it "parses 'guard has the silver coin'" $ do
            let result = parseCondPhrase "guard has the silver coin"
            result `shouldBe` Right (PossessiveExpression
                                    (SubjClause "guard")
                                    (PossessionClause "silver coin"))

        it "parses 'guard does not have a silver coin'" $ do
            let result = parseCondPhrase "guard does not have a silver coin"
            result `shouldBe` Right (NonPossessiveExpression
                                    (SubjClause "guard")
                                    (PossessionClause "silver coin"))

    describe "Condition Execution" $ do
        it "correctly evaluates when guard has coin" $ do
            let world = createTestWorld True
                hasCoin = PossessiveExpression
                            (SubjClause "guard")
                            (PossessionClause "silver coin")
                notHasCoin = NonPossessiveExpression
                                (SubjClause "guard")
                                (PossessionClause "silver coin")

            executeConditionCheck hasCoin world `shouldBe` True
            executeConditionCheck notHasCoin world `shouldBe` False

        it "correctly evaluates when guard does not have coin" $ do
            let world = createTestWorld False
                hasCoin = PossessiveExpression
                            (SubjClause "guard")
                            (PossessionClause "silver coin")
                notHasCoin = NonPossessiveExpression
                                (SubjClause "guard")
                                (PossessionClause "silver coin")

            executeConditionCheck hasCoin world `shouldBe` False
            executeConditionCheck notHasCoin world `shouldBe` True

        it "handles variations in condition text" $ do
            let world = createTestWorld False
                conditions =
                    [ "guard has silver coin"
                    , "guard has the silver coin"
                    , "guard does not have silver coin"
                    , "guard does not have a silver coin"
                    ]

            -- Test each condition string
            mapM_ (\condStr -> case parseCondPhrase condStr of
                Right cond -> do
                    let expected = "not" `elem` words condStr
                    executeConditionCheck cond world `shouldBe` expected
                Left err -> expectationFailure $ show $ renderExpressionError err
                ) conditions
