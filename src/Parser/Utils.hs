module Parser.Utils (getPrepVariants, getVerb, isPrepVariantOf) where

import Parser.Internal.Patterns (knownPreps,)
import           Data.Text    (Text, words)
import           Parser.Types
import           Prelude      hiding (words)


getPrepVariants :: Text -> Maybe [[Text]]
getPrepVariants basePrep = lookup basePrep knownPreps

isPrepVariantOf :: Text -> Text -> Bool
isPrepVariantOf basePrep variantPhrase = case getPrepVariants basePrep of
    Nothing -> False  -- Base preposition doesn't exist
    Just variants -> let words' = words variantPhrase
                    in words' `elem` variants

getVerb :: CmdExpression -> Text
getVerb = \case
    AtomicCmdExpression verb -> verb
    UnaryCmdExpression verb _ -> verb
    BinaryCmdExpression verb _ _ -> verb
    ComplexCmdExpression verb _ _ _ -> verb


