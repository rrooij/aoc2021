module Main where

import Data.List
import System.IO

-- Only part 1 yet!


countByFunction :: (a -> Bool) -> [a] -> Int
countByFunction f = length . filter f

zerosCount row = countByFunction (\x -> x == '0') row
zerosVerticalRows verticalRows = map (zerosCount) verticalRows
compareAmount verticalRows = length (head verticalRows) `div` 2
verticalRows contentLines = transpose contentLines

binaryStringToNumber :: String -> Int
binaryStringToNumber str = binaryStringToNumber_ (read str :: Int)
binaryStringToNumber_ 0 = 0
binaryStringToNumber_ i = 2 * binaryStringToNumber_ (div i 10) + (mod i 10)

rate :: [String] -> (Int -> Int -> Bool) -> Int
rate rows comparison = binaryStringToNumber binaryString
  where
    binaryString = map zeroOrOne (zerosVerticalRows rows)
    zeroOrOne zeroAmount
          | comparison zeroAmount (compareAmount rows) = '0'
          | otherwise = '1'

epsilonRate :: [String] -> Int
epsilonRate rows = rate rows (<)

gammaRate :: [String] -> Int
gammaRate rows = rate rows (>)

main = do
  handle <- openFile "./exercise3_input.txt" ReadMode
  contents <- hGetContents handle
  let contentLines = lines contents
  let rows = verticalRows contentLines
  print $ (epsilonRate rows) * (gammaRate rows)
  hClose handle
