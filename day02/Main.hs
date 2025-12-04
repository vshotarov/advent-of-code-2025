module Main where

import qualified Common
import Data.List.Split (splitOn)
import Data.List (nub)

main :: IO ()
main = do
    putStrLn $ "-- Solving day02 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let solve xToXs abToBounds (a,b) = 
            -- xToXs takes the current number in the bounds that we are checking
            -- and expands it to all possible repetitions of it:
            --   - in part1 that's just duplicating it once 
            --   - in part2 that is duplicating it until it's greater than b
            -- abToBounds takes the current range and produces the bounds
            -- to look for duplicating chunks in, i.e:
            --   - in part1 it's the first half of a to the first half of b
            --   - in part2 it's from 1 to the first half of b
            nub $ concat [xs | x <- [low..high], let xs = filterInRange $ xToXs x]
            where (low,high) = abToBounds (a,b)
                  filterInRange = filter (\x' -> x' >= a && x' <= b)

    let answer1 = sum $ concatMap (solve xToXs abToBounds) parsedInput
            where xToXs x = map read . filter (not . null)
                          $ [xstr ++ xstr, (init xstr) ++ (init xstr)]
                    where xstr = show x
                  abToBounds (a,b) = (a', if b' < a' then b'*10+9 else b')
                    where (a',b') = Common.mapTuple toFirstHalf (a,b)

    let answer2 = sum $ concatMap (\(a,b) -> solve (xToXs b) abToBounds (a,b)) parsedInput
            where xToXs b x = takeWhile (\n -> n <= b)
                            $ iterate ((+x) . (*d)) (x*d+x)
                                where d = 10 ^ (length $ show x)
                  abToBounds (_,b) = (1, toFirstHalf b)

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Range = (Int,Int)
parse :: String -> [Range]
parse = map (Common.mapTuple read . Common.splitOnceOn "-") . splitOn ","

toFirstHalf :: Int -> Int
toFirstHalf x = read $ take chunk xstring
    where xstring = show x
          xlen = length xstring
          chunk = xlen `div` 2 + (if even xlen then 0 else 1)

