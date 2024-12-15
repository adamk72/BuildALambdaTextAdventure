{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use !!" #-}
module Utils (ActorTag, ContainerTag, ItemTag, LocationTag, oxfordComma, oxfordEntityNames) where

import           Data.Text               (Text, intercalate, toLower)
import           Entity.Class.EntityBase
import           Entity.Entity           (Entity (..))

type LocationTag = Text

type ActorTag = Text

type ItemTag = Text

type ContainerTag = Text

oxfordEntityNames :: (HasEntityBase a) => [Entity a] -> Text
oxfordEntityNames = oxfordComma . map (toLower . getName)

oxfordComma :: [Text] -> Text
oxfordComma [] = ""
oxfordComma [x] = toLower x
oxfordComma [x, y] = toLower x <> " and " <> toLower y
oxfordComma xs =
  let init' = init xs
      last' = last xs
      commaList = intercalate ", " (map toLower init')
   in commaList <> ", and " <> toLower last'
