module Repl.ParserSpec (spec) where

import           Data.Text    (Text)
import           Repl.Parser
import           Test.Hspec

-- Helper for simple parse validation
testParseSuccess :: Text -> Spec
testParseSuccess input =
    it ("should successfully parse: " ++ show input) $ do
        case parseActionPhrase input of
            Just _ -> True `shouldBe` True
            Nothing -> expectationFailure $ "Expected successful parse but failed"

-- Helper for testing failed parses
testParseFail :: Text -> Spec
testParseFail input =
    it ("should fail to parse: " ++ show input) $ do
        case parseActionPhrase input of
            Just phrase -> expectationFailure $
                "Expected parse failure but got success: " ++ show phrase
            Nothing -> True `shouldBe` True

-- Helper for testing parsed components
testParseComponent :: Text -> (ActionPhrase -> Text) -> Text -> Spec
testParseComponent input extractor expected =
    it ("should extract " ++ show expected ++ " from: " ++ show input) $ do
        case parseActionPhrase input of
            Just result -> extractor result `shouldBe` expected
            Nothing -> expectationFailure $ "Expected successful parse but got Nothing"

spec :: Spec
spec = describe "Action Phrase Parser" $ do
    describe "Basic parsing patterns" $ do
        context "Simple cases with articles" $ do
            testParseSuccess "put the bauble in the bag"
            testParseSuccess "put a bauble in the bag"
            testParseSuccess "put the bauble in a bag"
            testParseSuccess "move bauble under bag"

        context "Complex object descriptions" $ do
            testParseSuccess "put the bauble in the brown bag"
            testParseSuccess "put the shiny bauble in the bag"
            testParseSuccess "put shiny bauble in brown bag"
            testParseSuccess "put shiny bauble in the brown bag"
            testParseSuccess "put the shiny bauble in brown bag"
            testParseSuccess "put the shiny bauble in the brown bag"

        context "Invalid commands" $ do
            testParseFail "the shiny bauble in the brown bag"  -- missing verb
            testParseFail "put in the brown bag"               -- missing object
            testParseFail "put bauble in"                      -- missing prep noun

    describe "Component extraction" $ do
        context "Object extraction" $ do
            testParseComponent "put ball in box" getObject "ball"
            testParseComponent "put the ball in the box" getObject "ball"

        context "Prepositional noun extraction" $ do
            testParseComponent "put ball in box" getPrepNoun "box"
            testParseComponent "place orb in container" getPrepNoun "container"

        context "Full phrase extraction" $ do
            testParseComponent "put the ball in the box" getRest "ball in box"

        context "Verb extraction" $ do
            testParseComponent "place the magic orb in the crystal box" getVerb "place"
            testParseComponent "move small token beside large token" getVerb "move"

        context "Preposition extraction" $ do
            testParseComponent "move small token beside large token" getPreposition "beside"
            testParseComponent "put ball in box" getPreposition "in"

    describe "Invalid inputs" $ do
        context "Empty or malformed input" $ do
            testParseFail "invalid input"
            testParseFail ""
            testParseFail "put"
            testParseFail "put in"
            testParseFail "in the box"