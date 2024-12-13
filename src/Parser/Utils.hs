module Parser.Utils (getPrepVariants, getVerb, isPrepVariantOf) where

import           Data.Text       (Text, words)
import           Parser.Patterns (knownPreps)
import           Parser.Types
import           Prelude         hiding (words)


getPrepVariants :: Text -> Maybe [[Text]]
getPrepVariants basePrep = lookup basePrep knownPreps

isPrepVariantOf :: Text -> Text -> Bool
isPrepVariantOf basePrep variantPhrase = case getPrepVariants basePrep of
    Nothing -> False
    Just variants -> let words' = words variantPhrase
                    in words' `elem` variants

getVerb :: CmdExpression -> Text
getVerb = \case
    AtomicCmdExpression verb -> verb
    UnaryCmdExpression verb _ -> verb
    SplitCmdExpression verb _ _ -> verb
    ComplexCmdExpression verb _ _ _ -> verb


