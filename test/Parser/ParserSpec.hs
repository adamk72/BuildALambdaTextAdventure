{-# LANGUAGE OverloadedStrings #-}
module Parser.ParserSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import Parser.Parser
import Parser.Types

spec :: Spec
spec = do
    describe "parsePhrase" $ do
        context "with single verb commands" $ do
            it "parses simple commands" $ do
                parsePhrase "inventory" `shouldBe` Right (AtomicExpression "inventory")
                parsePhrase "look" `shouldBe` Right (AtomicExpression "look")

            it "ignores case in verbs" $ do
                parsePhrase "INVENTORY" `shouldBe` Right (AtomicExpression "inventory")
                parsePhrase "Look" `shouldBe` Right (AtomicExpression "look")

            it "rejects unknown verbs" $ do
                parsePhrase "dance" `shouldBe` Left (UnknownVerb "dance")

        context "with verb-target commands" $ do
            it "parses simple movement commands" $ do
                parsePhrase "go cave" `shouldBe` Right (UnaryExpression "go" (NounClause "cave"))
                parsePhrase "go north" `shouldBe` Right (UnaryExpression "go" (NounClause "north"))

            it "handles articles in target" $ do
                parsePhrase "go the cave" `shouldBe` Right (UnaryExpression "go" (NounClause "cave"))
                parsePhrase "look north cave" `shouldBe` Right (UnaryExpression "look" (NounClause "north cave"))

            it "handles complex noun phrases in target" $ do
                parsePhrase "look ancient stone altar" `shouldBe`
                    Right (UnaryExpression "look" (NounClause "ancient stone altar"))
                parsePhrase "get mysterious glowing runes" `shouldBe`
                    Right (UnaryExpression "get" (NounClause "mysterious glowing runes"))

        context "with preposition commands" $ do
            it "parses simple preposition commands" $ do
                parsePhrase "look at bag" `shouldBe`
                    Right (BinaryExpression "look" (PrepClause "at") (NounClause "bag"))
                parsePhrase "look in box" `shouldBe`
                    Right (BinaryExpression "look" (PrepClause "in") (NounClause "box"))

            it "handles equivalent prepositions" $ do
                parsePhrase "look inside box" `shouldBe`
                    Right (BinaryExpression "look" (PrepClause "in") (NounClause "box"))
                parsePhrase "look into box" `shouldBe`
                    Right (BinaryExpression "look" (PrepClause "in") (NounClause "box"))

            it "handles complex phrases with prepositions" $ do
                parsePhrase "look at ancient stone altar" `shouldBe`
                    Right (BinaryExpression "look" (PrepClause "at") (NounClause "ancient stone altar"))
                parsePhrase "look under weathered marble statue" `shouldBe`
                    Right (BinaryExpression "look" (PrepClause "under") (NounClause "weathered marble statue"))

        context "with complex object commands" $ do
            it "parses basic object placement" $ do
                parsePhrase "put bauble in bag" `shouldBe`
                    Right (ComplexExpression "put" (NounClause "bauble") (PrepClause "in") (NounClause "bag"))
                parsePhrase "move key to box" `shouldBe`
                    Right (ComplexExpression "move" (NounClause "key") (PrepClause "to") (NounClause "box"))

            it "handles articles in both object and target" $ do
                parsePhrase "put the bauble in the bag" `shouldBe`
                    Right (ComplexExpression "put" (NounClause "bauble") (PrepClause "in") (NounClause "bag"))
                parsePhrase "move the orb onto the pedestal" `shouldBe`
                    Right (ComplexExpression "move" (NounClause "orb") (PrepClause "on") (NounClause "pedestal"))

            it "handles complex phrases in both object and target" $ do
                parsePhrase "put golden orb on marble pedestal" `shouldBe`
                    Right (ComplexExpression "put" (NounClause "golden orb") (PrepClause "on") (NounClause "marble pedestal"))
                parsePhrase "place ancient brass key in rusty lock" `shouldBe`
                    Right (ComplexExpression "place" (NounClause "ancient brass key") (PrepClause "in") (NounClause "rusty lock"))

        context "with invalid commands" $ do
            it "fails on empty input" $ do
                parsePhrase "" `shouldBe` Left (MalformedExpression "")

            it "fails on missing objects for verbs that require them" $ do
                parsePhrase "put in bag" `shouldBe` Left MissingObject
                parsePhrase "move to box" `shouldBe` Left MissingObject

            it "fails on missing targets" $ do
                parsePhrase "put bauble" `shouldBe` Left MissingTarget
                parsePhrase "look in" `shouldBe` Left MissingTarget

        context "expression rendering" $ do
            it "renders atomic expressions" $ do
                renderExpression (AtomicExpression "look") `shouldBe` "look"
                renderExpression (AtomicExpression "inventory") `shouldBe` "inventory"

            it "renders unary expressions" $ do
                renderExpression (UnaryExpression "go" (NounClause "cave"))
                    `shouldBe` "go cave"

            it "renders binary expressions" $ do
                renderExpression (BinaryExpression "look" (PrepClause "in") (NounClause "bag"))
                    `shouldBe` "look in bag"

            it "renders complex expressions" $ do
                renderExpression (ComplexExpression "put"
                                                  (NounClause "bauble")
                                                  (PrepClause "in")
                                                  (NounClause "bag"))
                    `shouldBe` "put bauble in bag"

            it "renders complex expressions with multi-word phrases" $ do
                renderExpression (ComplexExpression "place"
                                                  (NounClause "golden orb")
                                                  (PrepClause "on")
                                                  (NounClause "tall marble pedestal"))
                    `shouldBe` "place golden orb on tall marble pedestal"