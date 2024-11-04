{-# LANGUAGE OverloadedStrings #-}
module Utils (module Utils) where

import           Core.State
import           Data.Text  (Text, intercalate, toLower)

oxfordEntityNames :: [Entity] -> Text
oxfordEntityNames = oxfordComma . map (toLower . getName)

oxfordComma :: [Text] -> Text
oxfordComma [] = ""
oxfordComma [x] = toLower x
oxfordComma [x,y] = toLower x <> " and " <> toLower y
oxfordComma xs =
    -- Todo: Talk about init and last in blog
    let init' = init xs
        last' = last xs
        commaList = intercalate ", " (map toLower init')
    in commaList <> ", and " <> toLower last'
