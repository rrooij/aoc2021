module Main where

-- Run with: cat exercise2_input.txt | ./exercise 2

data SubmarinePosition = SubmarinePosition Int Int Int deriving (Show)

foldInstructionToSubmarine :: SubmarinePosition -> (String, Int) -> SubmarinePosition
foldInstructionToSubmarine (SubmarinePosition hozPos depth aim) ("forward", amount) = SubmarinePosition (hozPos + amount) depth aim
foldInstructionToSubmarine (SubmarinePosition hozPos depth aim)("down", amount) = SubmarinePosition hozPos (depth + amount) aim
foldInstructionToSubmarine (SubmarinePosition hozPos depth aim) ("up", amount) = SubmarinePosition hozPos (depth - amount) aim

foldInstructionToSubmarinePart2 :: SubmarinePosition -> (String, Int) -> SubmarinePosition
foldInstructionToSubmarinePart2 (SubmarinePosition hozPos depth aim) ("forward", amount)  = SubmarinePosition (hozPos + amount) (depth + (amount * aim)) aim
foldInstructionToSubmarinePart2 (SubmarinePosition hozPos depth aim) ("down", amount) = SubmarinePosition hozPos depth (aim + amount)
foldInstructionToSubmarinePart2 (SubmarinePosition hozPos depth aim) ("up", amount) = SubmarinePosition hozPos depth (aim - amount)

multiplied (SubmarinePosition x y _) = x * y

linesToProperTuple :: [String] -> [(String, Int)]
linesToProperTuple [] = []
linesToProperTuple (x:y:xs) = [(x, read y :: Int)] ++ linesToProperTuple xs

main = do
  content <- getContents
  let contentLines = words content
  let instructions = linesToProperTuple contentLines
  let submarine = foldl foldInstructionToSubmarine (SubmarinePosition 0 0 0) instructions
  print submarine
  putStrLn $ "Multiplied part one = " ++ show (multiplied submarine)
  let submarinePartTwo = foldl foldInstructionToSubmarinePart2 (SubmarinePosition 0 0 0) instructions
  print submarinePartTwo
  putStrLn $ "Multiplied part two = " ++ show (multiplied submarinePartTwo)
