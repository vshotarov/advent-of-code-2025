module Main where

import qualified Common
import Data.List.Split (splitOn, splitWhen)
import Data.List (transpose)

main :: IO ()
main = do
    putStrLn $ "-- Solving day06 --"
    input <- Common.readInput

    -- Solve
    let answer1 = solve1 input
    let answer2 = solve2 input

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Problem = ([Int] -> Int, [Int])

solve1 :: String -> Int
solve1 = sum
       . map (\t -> (if last t == "+" then sum else product) . map read $ init t)
       . transpose . map (filter (/="") . splitOn " ") . lines

solve2 :: String -> Int
solve2 = sum . map toProblem . splitWhen (all (==' ')) . transpose . lines
    where toProblem [] = error "bad input"
          toProblem (n:ns) = (if '+' `elem` n then sum else product) numbers
            where numbers = map read $ (filter (not . (`elem` "+*")) n):ns

