{-# LANGUAGE OverloadedStrings #-}

module Mock.TrialAdventureSpec (spec) where

import Test.Hspec
import Core.State.Entity
import Core.State.GameState
import Core.State.Location
import Core.State.TaggedEntity
import Data.Text (Text)

-- Test GameEnvironment configuration
testGameEnv :: GameEnvironment
testGameEnv = GameEnvironment
    { metadata = Metadata {title = "Trial of Your Life", launchTag = "Trial", description = "A short adventure for testing.", version = "1.0", author = "Adam Kecskes"}}
    { world = Just $ GameWorld
        { gwActiveActor = Entity {entityTag = TaggedEntity {tag = "alice", name = "Alice the Adventurer", location = Location {locTag = "meadow", locName = "A flowery meadow", destinationTags = ["cave"]}, inventory = Just Location {locTag = "alice", locName = "your pockets", destinationTags = []}}, entityType = ActorType}
        , gwPlayableActors = [Entity {entityTag = TaggedEntity {tag = "alice", name = "Alice the Adventurer", location = Location {locTag = "meadow", locName = "A flowery meadow", destinationTags = ["cave"]}, inventory = Just Location {locTag = "alice", locName = "your pockets", destinationTags = []}}, entityType = ActorType},Entity {entityTag = TaggedEntity {tag = "bob", name = "Bob the Brave", location = Location {locTag = "cave", locName = "A dark cave", destinationTags = ["meadow","forest"]}, inventory = Just Location {locTag = "bob", locName = "your pockets", destinationTags = []}}, entityType = ActorType}]
        , gwLocations = [Location {locTag = "meadow", locName = "A flowery meadow", destinationTags = ["cave"]},Location {locTag = "cave", locName = "A dark cave", destinationTags = ["meadow","forest"]},Location {locTag = "forest", locName = "A dense forest", destinationTags = ["cave"]}]
        , gwItems = [Entity {entityTag = TaggedEntity {tag = "silver coin", name = "a sliver coin", location = Location {locTag = "cave", locName = "A dark cave", destinationTags = ["meadow","forest"]}, inventory = Nothing}, entityType = ItemType},Entity {entityTag = TaggedEntity {tag = "tardis", name = "A large blue phone booth", location = Location {locTag = "meadow", locName = "A flowery meadow", destinationTags = ["cave"]}, inventory = Nothing}, entityType = ItemType},Entity {entityTag = TaggedEntity {tag = "magic globe", name = "A magic globe that you can see another world within", location = Location {locTag = "forest", locName = "A dense forest", destinationTags = ["cave"]}, inventory = Nothing}, entityType = ItemType},Entity {entityTag = TaggedEntity {tag = "bag of holding", name = "A satchel that is inky black on the inside", location = Location {locTag = "meadow", locName = "A flowery meadow", destinationTags = ["cave"]}, inventory = Nothing}, entityType = ItemType}]
        }
    }


spec :: Spec
spec = do
    describe "Game Configuration" $ do
        it "has correct metadata" $ do
            metadata testGameEnv `shouldBe` Metadata {title = "Trial of Your Life", launchTag = "Trial", description = "A short adventure for testing.", version = "1.0", author = "Adam Kecskes"}

        it "has correct active actor" $ do
            gwActiveActor <$> world testGameEnv `shouldBe` Just Entity {entityTag = TaggedEntity {tag = "alice", name = "Alice the Adventurer", location = Location {locTag = "meadow", locName = "A flowery meadow", destinationTags = ["cave"]}, inventory = Just Location {locTag = "alice", locName = "your pockets", destinationTags = []}}, entityType = ActorType}

        it "has expected number of playable actors" $ do
            length . gwPlayableActors <$> world testGameEnv `shouldBe` Just 2

        it "has expected number of locations" $ do
            length . gwLocations <$> world testGameEnv `shouldBe` Just 3

        it "has expected number of items" $ do
            length . gwItems <$> world testGameEnv `shouldBe` Just 4


