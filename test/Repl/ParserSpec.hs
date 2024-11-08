{-# LANGUAGE OverloadedStrings #-}
module Repl.ParserSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import Repl.Parser

spec :: Spec
spec = do
    describe "parsePhrase" $ do
        context "with single verb commands (V)" $ do
            it "parses simple commands" $ do
                parsePhrase "inventory" `shouldBe` Right (AtomicExpression "inventory")
                parsePhrase "look" `shouldBe` Right (AtomicExpression "look")

            it "ignores case in verbs" $ do
                parsePhrase "INVENTORY" `shouldBe` Right (AtomicExpression "inventory")
                parsePhrase "Look" `shouldBe` Right (AtomicExpression "look")

            it "rejects unknown verbs" $ do
                parsePhrase "dance" `shouldBe` Left (UnknownVerb "dance")

        context "with verb target commands (V T)" $ do
            it "parses simple movement commands" $ do
                parsePhrase "go cave" `shouldBe` Right (UnaryExpression "go" (Phrase "cave"))
                parsePhrase "go north" `shouldBe` Right (UnaryExpression "go" (Phrase "north"))

            it "handles articles in target" $ do
                parsePhrase "go the cave" `shouldBe` Right (UnaryExpression "go" (Phrase "cave"))
                parsePhrase "look north cave" `shouldBe` Right (UnaryExpression "look" (Phrase "north cave"))

            it "handles complex noun phrases in target" $ do
                parsePhrase "look ancient stone altar" `shouldBe`
                    Right (UnaryExpression "look" (Phrase "ancient stone altar"))
                parsePhrase "examine mysterious glowing runes" `shouldBe`
                    Right (UnaryExpression "examine" (Phrase "mysterious glowing runes"))

        context "with preposition commands (V P T)" $ do
            it "parses simple preposition commands" $ do
                parsePhrase "look at bag" `shouldBe` Right (PrepExpression "look" (Prep "at") (Phrase "bag"))
                parsePhrase "look in box" `shouldBe` Right (PrepExpression "look" (Prep "in") (Phrase "box"))

            it "handles compound prepositions" $ do
                parsePhrase "look inside of box" `shouldBe` Right (PrepExpression "look" (Prep "in") (Phrase "box"))
                parsePhrase "put on top of table" `shouldBe` Right (PrepExpression "put" (Prep "on") (Phrase "table"))

            it "handles complex noun phrases with prepositions" $ do
                parsePhrase "look at ancient stone altar" `shouldBe`
                    Right (PrepExpression "look" (Prep "at") (Phrase "ancient stone altar"))
                parsePhrase "search under weathered marble statue" `shouldBe`
                    Right (PrepExpression "search" (Prep "under") (Phrase "weathered marble statue"))

        context "with complex object commands (V O P T)" $ do
            it "parses basic object placement" $ do
                parsePhrase "put bauble in bag" `shouldBe`
                    Right (ComplexExpression "put" (Phrase "bauble") (Prep "in") (Phrase "bag"))
                parsePhrase "move key to box" `shouldBe`
                    Right (ComplexExpression "move" (Phrase "key") (Prep "to") (Phrase "box"))

            it "handles compound prepositions with objects" $ do
                parsePhrase "put cup on top of table" `shouldBe`
                    Right (ComplexExpression "put" (Phrase "cup") (Prep "on") (Phrase "table"))
                parsePhrase "move book onto shelf" `shouldBe`
                    Right (ComplexExpression "move" (Phrase "book") (Prep "on") (Phrase "shelf"))

            it "handles articles in both object and target" $ do
                parsePhrase "put the bauble in the bag" `shouldBe`
                    Right (ComplexExpression "put" (Phrase "bauble") (Prep "in") (Phrase "bag"))
                parsePhrase "take the key from the guard" `shouldBe`
                    Right (ComplexExpression "take" (Phrase "key") (Prep "from") (Phrase "guard"))

            it "handles complex noun phrases in both object and target" $ do
                parsePhrase "place golden orb on tall pedestal" `shouldBe`
                    Right (ComplexExpression "place" (Phrase "golden orb") (Prep "on") (Phrase "tall pedestal"))
                parsePhrase "put the ancient brass key in rusty iron lock" `shouldBe`
                    Right (ComplexExpression "put" (Phrase "ancient brass key") (Prep "in") (Phrase "rusty iron lock"))

            it "handles very complex noun phrases with compound prepositions" $ do
                parsePhrase "place the golden orb on top of the tall marble pedestal" `shouldBe`
                    Right (ComplexExpression "place" (Phrase "golden orb") (Prep "on") (Phrase "tall marble pedestal"))
                parsePhrase "put the small silver key inside of the ornate wooden box" `shouldBe`
                    Right (ComplexExpression "put" (Phrase "small silver key") (Prep "in") (Phrase "ornate wooden box"))

            it "handles multi-word descriptors in both object and target" $ do
                parsePhrase "move the half empty bottle onto the well worn shelf" `shouldBe`
                    Right (ComplexExpression "move" (Phrase "half empty bottle") (Prep "on") (Phrase "well worn shelf"))
                parsePhrase "place the brightly glowing crystal into the deep dark pit" `shouldBe`
                    Right (ComplexExpression "place" (Phrase "brightly glowing crystal") (Prep "in") (Phrase "deep dark pit"))

        context "with invalid commands" $ do
            it "fails on empty input" $ do
                parsePhrase "" `shouldBe` Left (MalformedExpression "")

            it "fails on missing objects for verbs that require them" $ do
                parsePhrase "put in bag" `shouldBe` Left MissingObject
                parsePhrase "move to box" `shouldBe` Left MissingObject

            it "fails on missing targets with complex objects" $ do
                parsePhrase "put ancient brass key" `shouldBe` Left MissingTarget
                parsePhrase "place the golden orb" `shouldBe` Left MissingTarget

            it "fails on missing targets" $ do
                parsePhrase "put bauble" `shouldBe` Left MissingTarget
                parsePhrase "look in" `shouldBe` Left MissingTarget

            it "fails on invalid prepositions" $ do
                parsePhrase "put bauble beside bag" `shouldBe` Left InvalidPrep
                parsePhrase "place golden orb against pedestal" `shouldBe` Left InvalidPrep

        context "expression rendering" $ do
            it "renders atomic expressions" $ do
                renderExpression (AtomicExpression "look") `shouldBe` "look"

            it "renders unary expressions" $ do
                renderExpression (UnaryExpression "go" (Phrase "cave")) `shouldBe` "go cave"

            it "renders prep expressions" $ do
                renderExpression (PrepExpression "look" (Prep "in") (Phrase "bag"))
                    `shouldBe` "look in bag"

            it "renders complex expressions" $ do
                renderExpression (ComplexExpression "put" (Phrase "bauble") (Prep "in") (Phrase "bag"))
                    `shouldBe` "put bauble in bag"

            it "renders complex expressions with multi-word phrases" $ do
                renderExpression (ComplexExpression "place"
                                                 (Phrase "golden orb")
                                                 (Prep "on")
                                                 (Phrase "tall marble pedestal"))
                    `shouldBe` "place golden orb on tall marble pedestal"