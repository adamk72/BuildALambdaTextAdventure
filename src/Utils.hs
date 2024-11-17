{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use !!" #-}
module Utils (module Utils) where

import           Data.Text     (Text, intercalate, toLower)
import qualified Data.Text     as T
import           Entity.Entity (getEntityName, Entity(..))

oxfordEntityNames :: [Entity a] -> Text
oxfordEntityNames = oxfordComma . map (toLower . getEntityName)

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

atMay :: [a] -> Int -> Maybe a
atMay xs n
    | n < 0 || n >= length xs = Nothing
    | otherwise = Just (head (drop n xs))

intToText :: Int -> Text
intToText = T.pack . show

maybeToEither :: Text -> Maybe a -> Either Text a
maybeToEither err Nothing = Left err
maybeToEither _ (Just x)  = Right x

