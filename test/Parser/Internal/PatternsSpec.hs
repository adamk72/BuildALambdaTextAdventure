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
            testPattern `shouldBe` Just (Possessive, ["guard"], ["silver", "coin"])

        it "identifies does not have patterns" $ do
            let testPattern = findPattern knownCondPatterns First
                    ["guard", "does", "not", "have", "silver", "coin"]
            testPattern `shouldBe` Just (NonPossessive, ["guard"], ["silver", "coin"])

        it "recognizes negation in patterns" $ do
            let str1 = words "guard does not have silver coin"
            let str2 = words "guard doesn't have silver coin"
            let str3 = words "guard does not have a silver coin"

            let pattern1 = findPattern knownCondPatterns First str1
            let pattern2 = findPattern knownCondPatterns First str2
            let pattern3 = findPattern knownCondPatterns First str3

            -- Just check that we get some kind of match for these patterns
            pattern1 `shouldSatisfy` \case
                Just (NonPossessive, _, _) -> True
                _ -> False

            pattern2 `shouldSatisfy` \case
                Just (NonPossessive, _, _) -> True
                _ -> False

            pattern3 `shouldSatisfy` \case
                Just (NonPossessive, _, _) -> True
                _ -> False

        it "includes negated forms in known patterns" $ do
            let negativePatterns = ["does not have", "doesn't have", "has no"]
            let allVariants = concat [variants | (base, variants) <- knownCondPatterns, base == NonPossessive]

            any (\negPattern ->
                    any (\variant ->
                        T.unwords variant == negPattern
                    ) allVariants
                ) negativePatterns `shouldBe` True
