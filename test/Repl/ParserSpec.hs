{-# LANGUAGE OverloadedStrings #-}
module Repl.ParserSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import Repl.Parser

spec :: Spec
spec = do
    describe "parseExpression" $ do
        context "with single verb commands (V)" $ do
            it "parses simple commands" $ do
                parseExpression "inventory" `shouldBe` Right (AtomicExpression "inventory")
                parseExpression "look" `shouldBe` Right (AtomicExpression "look")

            it "ignores case in verbs" $ do
                parseExpression "INVENTORY" `shouldBe` Right (AtomicExpression "inventory")
                parseExpression "Look" `shouldBe` Right (AtomicExpression "look")

            it "rejects unknown verbs" $ do
                parseExpression "dance" `shouldBe` Left (UnknownVerb "dance")

        context "with verb target commands (V T)" $ do
            it "parses simple movement commands" $ do
                parseExpression "go cave" `shouldBe` Right (UnaryExpression "go" (Phrase "cave"))
                parseExpression "go north" `shouldBe` Right (UnaryExpression "go" (Phrase "north"))

            it "handles articles in target" $ do
                parseExpression "go the cave" `shouldBe` Right (UnaryExpression "go" (Phrase "cave"))
                parseExpression "look north cave" `shouldBe` Right (UnaryExpression "look" (Phrase "north cave"))

            it "handles complex noun phrases in target" $ do
                parseExpression "look ancient stone altar" `shouldBe`
                    Right (UnaryExpression "look" (Phrase "ancient stone altar"))
                parseExpression "examine mysterious glowing runes" `shouldBe`
                    Right (UnaryExpression "examine" (Phrase "mysterious glowing runes"))

        context "with preposition commands (V P T)" $ do
            it "parses simple preposition commands" $ do
                parseExpression "look at bag" `shouldBe` Right (PrepExpression "look" (Prep "at") (Phrase "bag"))
                parseExpression "look in box" `shouldBe` Right (PrepExpression "look" (Prep "in") (Phrase "box"))

            it "handles compound prepositions" $ do
                parseExpression "look inside of box" `shouldBe` Right (PrepExpression "look" (Prep "in") (Phrase "box"))
                parseExpression "put on top of table" `shouldBe` Right (PrepExpression "put" (Prep "on") (Phrase "table"))

            it "handles complex noun phrases with prepositions" $ do
                parseExpression "look at ancient stone altar" `shouldBe`
                    Right (PrepExpression "look" (Prep "at") (Phrase "ancient stone altar"))
                parseExpression "search under weathered marble statue" `shouldBe`
                    Right (PrepExpression "search" (Prep "under") (Phrase "weathered marble statue"))

        context "with complex object commands (V O P T)" $ do
            it "parses basic object placement" $ do
                parseExpression "put bauble in bag" `shouldBe`
                    Right (ComplexExpression "put" (Phrase "bauble") (Prep "in") (Phrase "bag"))
                parseExpression "move key to box" `shouldBe`
                    Right (ComplexExpression "move" (Phrase "key") (Prep "to") (Phrase "box"))

            it "handles compound prepositions with objects" $ do
                parseExpression "put cup on top of table" `shouldBe`
                    Right (ComplexExpression "put" (Phrase "cup") (Prep "on") (Phrase "table"))
                parseExpression "move book onto shelf" `shouldBe`
                    Right (ComplexExpression "move" (Phrase "book") (Prep "on") (Phrase "shelf"))

            it "handles articles in both object and target" $ do
                parseExpression "put the bauble in the bag" `shouldBe`
                    Right (ComplexExpression "put" (Phrase "bauble") (Prep "in") (Phrase "bag"))
                parseExpression "take the key from the guard" `shouldBe`
                    Right (ComplexExpression "take" (Phrase "key") (Prep "from") (Phrase "guard"))

            it "handles complex noun phrases in both object and target" $ do
                parseExpression "place golden orb on tall pedestal" `shouldBe`
                    Right (ComplexExpression "place" (Phrase "golden orb") (Prep "on") (Phrase "tall pedestal"))
                parseExpression "put the ancient brass key in rusty iron lock" `shouldBe`
                    Right (ComplexExpression "put" (Phrase "ancient brass key") (Prep "in") (Phrase "rusty iron lock"))

            it "handles very complex noun phrases with compound prepositions" $ do
                parseExpression "place the golden orb on top of the tall marble pedestal" `shouldBe`
                    Right (ComplexExpression "place" (Phrase "golden orb") (Prep "on") (Phrase "tall marble pedestal"))
                parseExpression "put the small silver key inside of the ornate wooden box" `shouldBe`
                    Right (ComplexExpression "put" (Phrase "small silver key") (Prep "in") (Phrase "ornate wooden box"))

            it "handles multi-word descriptors in both object and target" $ do
                parseExpression "move the half empty bottle onto the well worn shelf" `shouldBe`
                    Right (ComplexExpression "move" (Phrase "half empty bottle") (Prep "on") (Phrase "well worn shelf"))
                parseExpression "place the brightly glowing crystal into the deep dark pit" `shouldBe`
                    Right (ComplexExpression "place" (Phrase "brightly glowing crystal") (Prep "in") (Phrase "deep dark pit"))

        context "with invalid commands" $ do
            it "fails on empty input" $ do
                parseExpression "" `shouldBe` Left (MalformedExpression "")

            it "fails on missing objects for verbs that require them" $ do
                parseExpression "put in bag" `shouldBe` Left MissingObject
                parseExpression "move to box" `shouldBe` Left MissingObject

            it "fails on missing targets with complex objects" $ do
                parseExpression "put ancient brass key" `shouldBe` Left MissingTarget
                parseExpression "place the golden orb" `shouldBe` Left MissingTarget

            it "fails on missing targets" $ do
                parseExpression "put bauble" `shouldBe` Left MissingTarget
                parseExpression "look in" `shouldBe` Left MissingTarget

            it "fails on invalid prepositions" $ do
                parseExpression "put bauble beside bag" `shouldBe` Left InvalidPrep
                parseExpression "place golden orb against pedestal" `shouldBe` Left InvalidPrep

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