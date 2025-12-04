module Main where

import qualified Common
import Data.Maybe (fromJust)
import Data.List (elemIndex)

main :: IO ()
main = do
    putStrLn $ "-- Solving day03 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let solveN n = sum $ map (comboToJoltage . getHighestJoltageComboOfSize n)
                             parsedInput
    let answer1 = solveN 2
    let answer2 = solveN 12

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Bank = [Int]

parse :: String -> [Bank]
parse input = map (map (read . (:[]))) $ lines input

getHighestJoltageComboOfSize :: Int -> Bank -> [Int]
getHighestJoltageComboOfSize n bank
  | length bank < n = []
  | n == 1 = [maximum bank]
  | otherwise = go startMB startI
  where getMaxAndItsIndex bank' = (maxBank, fromJust $ elemIndex maxBank bank)
            where maxBank = maximum bank'
        (startMB,startI) = getMaxAndItsIndex bank
        go mb i
          | length combo == n-1 = [mb] ++ combo
          | null possible = []
          | otherwise = go mb' i'
          where possible = filter (<mb) bank
                (mb',i') = getMaxAndItsIndex possible
                combo = getHighestJoltageComboOfSize (n-1) (drop (i+1) bank)

comboToJoltage :: [Int] -> Int
comboToJoltage combo = sum . map (\(c,p) -> c*(10^p)) $ zip combo [len-1,len-2..0]
    where len = length combo

