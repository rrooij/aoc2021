module Main where

-- Run with: cat exercise2_input.txt | ./exercise 2

data SubmarinePosition = SubmarinePosition Int Int deriving (Show)

foldInstructionToSubmarine  ("forward", amount) (SubmarinePosition hozPos depth) = SubmarinePosition (hozPos + amount) depth
foldInstructionToSubmarine ("down", amount) (SubmarinePosition hozPos depth) = SubmarinePosition hozPos (depth + amount)
foldInstructionToSubmarine ("up", amount) (SubmarinePosition hozPos depth) = SubmarinePosition hozPos (depth - amount)

multiplied (SubmarinePosition x y) = x * y

linesToProperTuple :: [String] -> [(String, Int)]
linesToProperTuple [] = []
linesToProperTuple (x:y:xs) = [(x, read y :: Int)] ++ linesToProperTuple xs

main = do
  content <- getContents
  let contentLines = words content
  let instructions = linesToProperTuple contentLines
  let submarine = foldr foldInstructionToSubmarine (SubmarinePosition 0 0) instructions
  print submarine
  putStrLn $ "Multiplied = " ++ show (multiplied submarine)
