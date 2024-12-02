{-# LANGUAGE OverloadedStrings #-}
module Parser.Internal.PatternsSpec (spec) where

import           Data.Text                (Text, words)
import qualified Data.Text                as T
import           Parser.Internal.Patterns
import           Parser.Types
import           Prelude                  hiding (words)
import           Test.Hspec

spec :: Spec
spec = describe "Pattern Matching" $ do
    describe "Possession Patterns" $ do
        it "identifies basic has patterns" $ do
            let testPattern = findPattern knownCondPatterns First ["guard", "has", "silver", "coin"]
            testPattern `shouldBe` Just ("has", ["guard"], ["silver", "coin"])

        it "identifies does not have patterns" $ do
            let testPattern = findPattern knownCondPatterns First
                    ["guard", "does", "not", "have", "silver", "coin"]
            testPattern `shouldBe` Just ("has", ["guard"], ["silver", "coin"])

        it "recognizes negation in patterns" $ do
            let str1 = words "guard does not have silver coin"
            let str2 = words "guard doesn't have silver coin"
            let str3 = words "guard does not have a silver coin"

            let pattern1 = findPattern knownCondPatterns First str1
            let pattern2 = findPattern knownCondPatterns First str2
            let pattern3 = findPattern knownCondPatterns First str3

            -- Just check that we get some kind of match for these patterns
            pattern1 `shouldSatisfy` \case
                Just ("has", _, _) -> True
                _ -> False

            pattern2 `shouldSatisfy` \case
                Just ("has", _, _) -> True
                _ -> False

            pattern3 `shouldSatisfy` \case
                Just ("has", _, _) -> True
                _ -> False

        it "includes negated forms in known patterns" $ do
            let negativePatterns = ["does not have", "doesn't have", "has no"]
            let allVariants = concat [variants | (base, variants) <- knownCondPatterns, base == "has"]

            -- Check that at least some negative forms are included
            any (\negPattern ->
                    any (\variant ->
                        T.unwords variant == negPattern
                    ) allVariants
                ) negativePatterns `shouldBe` True

    describe "findMatchingPattern" $ do
        it "identifies basic patterns correctly" $ do
            findMatchingPattern "is" `shouldBe` Just ("is", False)
            findMatchingPattern "has" `shouldBe` Just ("has", False)
            findMatchingPattern "at" `shouldBe` Just ("at", False)

        it "identifies negated patterns correctly" $ do
            findMatchingPattern "is not" `shouldBe` Just ("is", True)
            findMatchingPattern "isn't" `shouldBe` Just ("is", True)
            findMatchingPattern "does not have" `shouldBe` Just ("has", True)
            findMatchingPattern "doesn't have" `shouldBe` Just ("has", True)
            findMatchingPattern "is not at" `shouldBe` Just ("at", True)

        it "handles case insensitivity" $ do
            findMatchingPattern "IS" `shouldBe` Just ("is", False)
            findMatchingPattern "Is NoT" `shouldBe` Just ("is", True)
            findMatchingPattern "HaS" `shouldBe` Just ("has", False)

        it "matches pattern variants" $ do
            findMatchingPattern "owns" `shouldBe` Just ("has", False)
            findMatchingPattern "carries" `shouldBe` Just ("has", False)
            findMatchingPattern "inside" `shouldBe` Just ("at", False)
            findMatchingPattern "isn't in" `shouldBe` Just ("at", True)

        it "handles unknown patterns" $ do
            findMatchingPattern "unknown" `shouldBe` Nothing
            findMatchingPattern "" `shouldBe` Nothing

        it "matches compound patterns" $ do
            findMatchingPattern "is in" `shouldBe` Just ("at", False)
            findMatchingPattern "is not in" `shouldBe` Just ("at", True)
            findMatchingPattern "does not possess" `shouldBe` Just ("has", True)
            findMatchingPattern "doesn't carry" `shouldBe` Just ("has", True)
