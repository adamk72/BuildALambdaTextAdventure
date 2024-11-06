module Repl.ParserSpec (spec) where

import           Data.Text
import           Repl.Parser
import           Test.Hspec

testParse :: Text -> Bool
testParse input = case parseActionPhrase input of
    Just _  -> True
    Nothing -> False

spec :: Spec
spec = describe "Parsing test" $ do
  context "Check for verb-noun-preposition-noun + articles" $ do
    it "should handle simple cases" $ do
      testParse "put the bauble in the bag" `shouldBe` True
      testParse "put a bauble in the bag" `shouldBe` True
      testParse "put the bauble in a bag" `shouldBe` True
      testParse "move bauble under bag" `shouldBe` True
    it "should handle sentence with complex objects" $ do
      testParse "put the bauble in the brown bag" `shouldBe` True
      testParse "put the shiny bauble in the bag" `shouldBe` True
      testParse "put shiny bauble in brown bag" `shouldBe` True
      testParse "put shiny bauble in the brown bag" `shouldBe` True
      testParse "put the shiny bauble in brown bag" `shouldBe` True
      testParse "put the shiny bauble in the brown bag" `shouldBe` True
    it "should handle badly formed sentences" $ do
      testParse "the shiny bauble in the brown bag" `shouldBe` True
      testParse "put in the brown bag" `shouldBe` True
      testParse "put bauble in" `shouldBe` True
