module Main where

import Data.List
import Data.List.Split
import Data.Ord
import System.IO

data Board = Board { horizontalRows :: [[Int]], verticalRows :: [[Int]] } deriving (Show)
data WinningBoard = WinningBoard { board :: Board, calledNumber :: Int, tries :: Int } deriving (Show)

guessedNumbers :: [String] -> [Int]
guessedNumbers (line:lines) = (map (\x -> read x :: Int) . splitOn ",") line

filterBoard :: Board -> Int -> Board
filterBoard (Board horRows vertRows ) number = Board (mapFilter horRows) (mapFilter vertRows)
  where mapFilter rows = map (filter (/= number)) rows

triesUntilSuccess :: Board -> [Int] -> Int -> WinningBoard -- board, tries and winning number
triesUntilSuccess board [] number = WinningBoard board 0 0 -- no success
triesUntilSuccess (Board horRows vertRows) (x:xs) tries
  | elem [] (verticalRows newBoard) || elem [] (horizontalRows newBoard) = WinningBoard newBoard x tries
  | otherwise = triesUntilSuccess newBoard xs (tries + 1)
  where newBoard = filterBoard (Board horRows vertRows) x

sumOfBoard :: Board -> Int
sumOfBoard (Board horRows _) = sumOfRows horRows
  where sumOfRows rows = sum $ map sum rows

extractStringBoards contentLines = map words (drop 2 contentLines)
convertStringBoardsToInteger :: [[String]] -> [[Int]]
convertStringBoardsToInteger = map (map (\x -> read x :: Int))
extractBoards contentLines = map convertBoardType boardLists
  where integerBoards = convertStringBoardsToInteger (extractStringBoards contentLines)
        boardLists = splitOn [[]] integerBoards
        convertBoardType board = Board board (transpose board)


main = do
  handle <- openFile "./exercise4_input.txt" ReadMode
  contents <- hGetContents handle
  let contentLines = lines contents
  let guessedNumberList = guessedNumbers contentLines
  let boards = extractBoards contentLines
  let boardTries = map (\board -> triesUntilSuccess board guessedNumberList 0) boards
  let winningBoard = minimumBy (comparing tries) boardTries
  let answerPart1 = sumOfBoard (board winningBoard) * calledNumber winningBoard
  print answerPart1
