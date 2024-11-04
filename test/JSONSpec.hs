{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module JSONSpec (spec) where

import           Core.State.Entity
import           Core.State.JSON
import            Core.State.GameState
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
    "startingCharacter": "alice",
    "gwPlayableCharacters": [
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
    "gwInteractables": [
      {
        "tag": "silver coin",
        "name": "a sliver coin",
        "locationTag": "cave"
      }
    ],
    "gwLocations": [
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
    "startingCharacter": "frank",
    "gwPlayableCharacters": [
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
    "gwInteractables": [
      {
        "tag": "silver coin",
        "name": "a sliver coin",
        "locationTag": "cave"
      }
    ],
    "gwLocations": [
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
spec = describe "GameWorld JSON Parsing" $ do
    context "Valid JSON parsing" $ do
        it "successfully parses" $ do
            let result = eitherDecode validJson :: Either String GameEnvironmentJSON
            result `shouldSatisfy` isRight

        it "starting character is a Character type" $ do
            let Right gameEnvJSON = eitherDecode validJson
            case world (unGameEnvironment gameEnvJSON) of
                Just gameWorld -> do
                    let startingChar = gwActiveCharacter gameWorld
                    startingChar `shouldSatisfy` isCharacter
                Nothing -> expectationFailure "Expected GameWorld to be present"

        it "succeeds when starting character exists in playable characters list" $ do
            let Right gameEnvJSON = eitherDecode validJson
            case world (unGameEnvironment gameEnvJSON) of
                Just gameWorld -> do
                    let startingCharTag = getTag (gwActiveCharacter gameWorld)
                    let playableChars = gwPlayableCharacters gameWorld
                    let foundChar = any (\char -> getTag char == startingCharTag) playableChars
                    foundChar `shouldBe` True
                Nothing -> expectationFailure "Expected GameWorld to be present"

    context "Invalid JSON parsing" $ do
        it "provides appropriate error message for missing starting character" $ do
            let result = eitherDecode invalidJson :: Either String GameEnvironmentJSON
            case result of
                Left err -> err `shouldContain` "Starting character with tag"
                Right _ -> expectationFailure "Expected parsing to fail but it succeeded"

