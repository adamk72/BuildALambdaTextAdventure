--TextReflow.hs
--Copyright Laurence Emms 2018
--Module to reflow text to a specific column limit

module TextReflow (joinStrings,
                   reflowLine,
                   reflowLines,
                   reflowPutStr,
                   reflowPutStrs,
                   reflowString,
                   reflowStrings) where

import qualified Data.List.Split as D
import           Prelude         hiding (lines, words)
import PrintParams (allDelimiters, allColumnWidth)

joinStrings :: String -> [String] -> String
joinStrings _ [] = ""
joinStrings joinString (string : strings) = string ++ joinString ++ joinStrings joinString strings

--Reflow a single line split into words by delimiters
reflowLine :: Int -> Int -> [String] -> [String] -> [String]
reflowLine _ _ [] result = reverse result
reflowLine c columnWidth (word : words) []
    | c + length word < columnWidth = reflowLine (c + length word) columnWidth words [word]
    | otherwise = reflowLine 0 columnWidth words [word]
reflowLine c columnWidth (word : words) (line : result)
    | c + length word < columnWidth = reflowLine (c + length word) columnWidth words ((line ++ word) : result)
    | otherwise = reflowLine 0 columnWidth words (word : line : result)

--Reflow a set of lines
reflowLines :: String -> Int -> [String] -> [String]
reflowLines _ _ [] = [] --No lines to reflow
reflowLines delimiters columnWidth (line : lines)
    = reflowLine 0 columnWidth (D.split (D.keepDelimsR $ D.oneOf delimiters) line) [] ++ reflowLines delimiters columnWidth lines

--Insert newlines into the reflowed lines, ignoring all lines which are followed by a delimiter line
intercalateNewlines :: [String] -> String
intercalateNewlines [] = ""
intercalateNewlines [line] = line
intercalateNewlines (line1 : line2 : linesRemaining)
    | not (null line2) && head line2 == '\n' = line1 ++ line2 ++ intercalateNewlines linesRemaining --If the next line is a delimiter, just concatenate the lines
    | otherwise = line1 ++ "\n" ++ intercalateNewlines (line2 : linesRemaining) --If the next line is not a delimiter, add one

reflowPutStrAux :: String -> Int -> String -> IO ()
reflowPutStrAux delimiters columnWidth line
    = putStr (intercalateNewlines (reflowLines delimiters columnWidth lines))
        where lines = D.split (D.keepDelimsR $ D.oneOf "\n") line --Split into lines keeping existing newlines
reflowPutStr :: String -> IO ()
reflowPutStr = reflowPutStrAux allDelimiters allColumnWidth

reflowPutStrs :: [String] -> IO ()
reflowPutStrs lines = reflowPutStr (concat lines)

reflowString :: String -> Int -> String -> [String]
reflowString delimiters columnWidth string
    = reflowLines delimiters columnWidth lines
        where lines = D.split (D.keepDelimsR $ D.oneOf "\n") string

reflowStrings :: String -> Int -> [String] -> [String]
reflowStrings delimiters columnWidth strings = reflowString delimiters columnWidth (concat strings)
