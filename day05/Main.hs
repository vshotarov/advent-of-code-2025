module Main where

import qualified Common
import Data.List (nub)
import Data.Maybe (isNothing, fromJust)

main :: IO ()
main = do
    putStrLn $ "-- Solving day05 --"
    input <- Common.readInput

    -- Parse
    let parsedInput@(ranges,ids) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = length $ filter (\iid -> any (isInRange iid) ranges) ids
    let answer2 = sum . map (\(a,b) -> b+1-a) . go $ nub ranges
            where unions rs = [(a,b,fromJust u) | a <- rs, b <- rs,
                               let u = union a b, a/=b, not $ isNothing u]
                  go rs = case unions rs of
                            [] -> rs
                            (a,b,u):_ -> go $ u:(filter (not . (`elem` [a,b,u])) rs)

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Range = (Int,Int)

parse :: String -> ([Range], [Int])
parse input = (map parseRange ranges, map read ids)
    where (ranges,ids) = Common.splitOnceOn [""] $ lines input
          parseRange = Common.mapTuple read . Common.splitOnceOn "-"

isInRange :: Int -> Range -> Bool
isInRange x (a,b) = x >= a && x <= b

union :: Range -> Range -> Maybe Range
union (a,b) (c,d)
  | b < c || a > d = Nothing
  | otherwise = Just (min a c, max b d)

