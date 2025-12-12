module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day12 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = length $ filter (\((w,h),ps) -> (w `div` 3)*(h `div` 3) >= sum ps) parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1

parse :: String -> [((Int,Int),[Int])]
parse input = map parseRegion . filter ('x' `elem`) $ lines input
    where parseRegion l = ((read w, read h), map read $ words ps)
              where (size,ps) = Common.splitOnceOn ": " l
                    (w,h) = Common.splitOnceOn "x" size

