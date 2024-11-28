{-# LANGUAGE OverloadedStrings #-}
module Parser.ParserSpec (spec) where

import           Parser.Parser
import           Parser.Types
import           Test.Hspec

spec :: Spec
spec = do
    describe "parseCmdPhrase" $ do
        context "with single verb commands" $ do
            it "parses simple commands" $ do
                parseCmdPhrase "inventory" `shouldBe` Right (AtomicCmdExpression "inventory")
                parseCmdPhrase "look" `shouldBe` Right (AtomicCmdExpression "look")

            it "ignores case in verbs" $ do
                parseCmdPhrase "INVENTORY" `shouldBe` Right (AtomicCmdExpression "inventory")
                parseCmdPhrase "Look" `shouldBe` Right (AtomicCmdExpression "look")

            it "rejects unknown verbs" $ do
                parseCmdPhrase "dance" `shouldBe` Left (UnknownVerb "dance")

        context "with verb-target commands" $ do
            it "parses simple movement commands" $ do
                parseCmdPhrase "go cave" `shouldBe` Right (UnaryCmdExpression "go" (NounClause "cave"))
                parseCmdPhrase "go north" `shouldBe` Right (UnaryCmdExpression "go" (NounClause "north"))

            it "handles articles in target" $ do
                parseCmdPhrase "go the cave" `shouldBe` Right (UnaryCmdExpression "go" (NounClause "cave"))
                parseCmdPhrase "look north cave" `shouldBe` Right (UnaryCmdExpression "look" (NounClause "north cave"))

            it "handles complex noun phrases in target" $ do
                parseCmdPhrase "look ancient stone altar" `shouldBe`
                    Right (UnaryCmdExpression "look" (NounClause "ancient stone altar"))
                parseCmdPhrase "get mysterious glowing runes" `shouldBe`
                    Right (UnaryCmdExpression "get" (NounClause "mysterious glowing runes"))

        context "with preposition commands" $ do
            it "parses simple preposition commands" $ do
                parseCmdPhrase "look at bag" `shouldBe`
                    Right (BinaryCmdExpression "look" (PrepClause "at") (NounClause "bag"))
                parseCmdPhrase "look in box" `shouldBe`
                    Right (BinaryCmdExpression "look" (PrepClause "in") (NounClause "box"))

            it "handles equivalent prepositions" $ do
                parseCmdPhrase "look inside box" `shouldBe`
                    Right (BinaryCmdExpression "look" (PrepClause "in") (NounClause "box"))
                parseCmdPhrase "look into box" `shouldBe`
                    Right (BinaryCmdExpression "look" (PrepClause "in") (NounClause "box"))

            it "handles complex phrases with prepositions" $ do
                parseCmdPhrase "look at ancient stone altar" `shouldBe`
                    Right (BinaryCmdExpression "look" (PrepClause "at") (NounClause "ancient stone altar"))
                parseCmdPhrase "look under weathered marble statue" `shouldBe`
                    Right (BinaryCmdExpression "look" (PrepClause "under") (NounClause "weathered marble statue"))

        context "with complex object commands" $ do
            it "parses basic object placement" $ do
                parseCmdPhrase "put bauble in bag" `shouldBe`
                    Right (ComplexCmdExpression "put" (NounClause "bauble") (PrepClause "in") (NounClause "bag"))
                parseCmdPhrase "move key to box" `shouldBe`
                    Right (ComplexCmdExpression "move" (NounClause "key") (PrepClause "to") (NounClause "box"))

            it "handles articles in both object and target" $ do
                parseCmdPhrase "put the bauble in the bag" `shouldBe`
                    Right (ComplexCmdExpression "put" (NounClause "bauble") (PrepClause "in") (NounClause "bag"))
                parseCmdPhrase "move the orb onto the pedestal" `shouldBe`
                    Right (ComplexCmdExpression "move" (NounClause "orb") (PrepClause "on") (NounClause "pedestal"))

            it "handles complex phrases in both object and target" $ do
                parseCmdPhrase "put golden orb on marble pedestal" `shouldBe`
                    Right (ComplexCmdExpression "put" (NounClause "golden orb") (PrepClause "on") (NounClause "marble pedestal"))
                parseCmdPhrase "place ancient brass key in rusty lock" `shouldBe`
                    Right (ComplexCmdExpression "place" (NounClause "ancient brass key") (PrepClause "in") (NounClause "rusty lock"))

        context "with invalid commands" $ do
            it "fails on empty input" $ do
                parseCmdPhrase "" `shouldBe` Left (MalformedExpression "")

            it "fails on missing objects for verbs that require them" $ do
                parseCmdPhrase "put in bag" `shouldBe` Left MissingObject
                parseCmdPhrase "move to box" `shouldBe` Left MissingObject

            it "fails on missing targets" $ do
                parseCmdPhrase "put bauble" `shouldBe` Left MissingTarget
                parseCmdPhrase "look in" `shouldBe` Left MissingTarget

        context "expression rendering" $ do
            it "renders atomic expressions" $ do
                renderExpression (AtomicCmdExpression "look") `shouldBe` "look"
                renderExpression (AtomicCmdExpression "inventory") `shouldBe` "inventory"

            it "renders unary expressions" $ do
                renderExpression (UnaryCmdExpression "go" (NounClause "cave"))
                    `shouldBe` "go cave"

            it "renders binary expressions" $ do
                renderExpression (BinaryCmdExpression "look" (PrepClause "in") (NounClause "bag"))
                    `shouldBe` "look in bag"

            it "renders complex expressions" $ do
                renderExpression (ComplexCmdExpression "put"
                                                  (NounClause "bauble")
                                                  (PrepClause "in")
                                                  (NounClause "bag"))
                    `shouldBe` "put bauble in bag"

            it "renders complex expressions with multi-word phrases" $ do
                renderExpression (ComplexCmdExpression "place"
                                                  (NounClause "golden orb")
                                                  (PrepClause "on")
                                                  (NounClause "tall marble pedestal"))
                    `shouldBe` "place golden orb on tall marble pedestal"
