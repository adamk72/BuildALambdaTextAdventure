{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module JSONSpec (spec) where

import           Entity.Entity
import           Core.State.GameState
import           Core.State.JSON
import           Core.State.TaggedEntity
import           Data.Aeson
import           Data.ByteString.Lazy    (ByteString)
import           Data.Either
import           Test.Hspec
import           Text.RawString.QQ

validJson :: ByteString
validJson = [r|
{
  "metadata": {
    "title": "Trial of Your Life",
    "launchTag": "Trial",
    "description": "A short adventure for testing.",
    "version": "1.0",
    "author": "Adam Kecskes"
  },
  "world": {
    "startingActor": "alice",
    "characters": [
      {
        "tag": "alice",
        "name": "Alice the Adventurer",
        "locationTag": "meadow"
      },
      {
        "tag": "bob",
        "name": "Bob the Brave",
        "locationTag": "cave"
      }
    ],
    "items": [
      {
        "tag": "silver coin",
        "name": "a silver coin",
        "locationTag": "cave"
      }
    ],
    "locations": [
      {
        "tag": "meadow",
        "name": "A flowery meadow",
        "destinationTags": [
          "cave"
        ]
      },
      {
        "tag": "cave",
        "name": "A dark cave",
        "destinationTags": [
          "meadow",
          "forest"
        ]
      },
      {
        "tag": "forest",
        "name": "A dense forest",
        "destinationTags": [
          "cave"
        ]
      }
    ]
  }
}
|]

invalidJson :: ByteString
invalidJson = [r|
{
  "metadata": {
    "title": "Trial of Your Life",
    "launchTag": "Trial",
    "description": "A short adventure for testing.",
    "version": "1.0",
    "author": "Adam Kecskes"
  },
  "world": {
    "startingActor": "frank",
    "characters": [
      {
        "tag": "alice",
        "name": "Alice the Adventurer",
        "locationTag": "meadow"
      },
      {
        "tag": "bob",
        "name": "Bob the Brave",
        "locationTag": "cave"
      }
    ],
    "items": [
      {
        "tag": "silver coin",
        "name": "a silver coin",
        "locationTag": "cave"
      }
    ],
    "locations": [
      {
        "tag": "meadow",
        "name": "A flowery meadow",
        "destinationTags": [
          "cave"
        ]
      },
      {
        "tag": "cave",
        "name": "A dark cave",
        "destinationTags": [
          "meadow",
          "forest"
        ]
      },
      {
        "tag": "forest",
        "name": "A dense forest",
        "destinationTags": [
          "cave"
        ]
      }
    ]
  }
}
|]

spec :: Spec
spec = describe "World JSON Parsing" $ do
    context "Valid JSON parsing" $ do
        it "successfully parses" $ do
            let result = eitherDecode validJson :: Either String GameEnvironmentJSON
            result `shouldSatisfy` isRight

        it "starting character is a Actor type" $ do
            let Right gameEnvJSON = eitherDecode validJson
            case world (unGameEnvironment gameEnvJSON) of
                Just gw -> do
                    let startingActor = gwActiveActor gw
                    startingActor `shouldSatisfy` isActor
                Nothing -> expectationFailure "Expected World to be present"

        it "succeeds when starting character exists in playable characters list" $ do
            let Right gameEnvJSON = eitherDecode validJson
            case world (unGameEnvironment gameEnvJSON) of
                Just gw -> do
                    let startingActorTag = getId (gwActiveActor gw)
                    let playableActors = gwPlayableActors gw
                    let foundActor = any (\actor -> getId actor == startingActorTag) playableActors
                    foundActor `shouldBe` True
                Nothing -> expectationFailure "Expected World to be present"

    context "Invalid JSON parsing" $ do
        it "provides appropriate error message for missing starting character" $ do
            let result = eitherDecode invalidJson :: Either String GameEnvironmentJSON
            case result of
                Left err -> err `shouldContain` "Starting character with tag"
                Right _  -> expectationFailure "Expected parsing to fail but it succeeded"

