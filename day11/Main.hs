module Main where

import qualified Common
import Data.List.Split (splitOn)
import qualified Data.Map as M

main :: IO ()
main = do
    putStrLn $ "-- Solving day11 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = getNumOfAllPathsFromAToB parsedInput "you" "out"
    let answer2 = product . map (uncurry (getNumOfAllPathsFromAToB parsedInput))
                $ [("svr","fft"),("fft","dac"),("dac","out")]

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Network = M.Map String [String]

parse :: String -> Network
parse input = M.fromList . map parseLine $ lines input
    where parseLine l = let (src,dests) = Common.splitOnceOn ": " l
                         in (src, splitOn " " dests)

getNumOfAllPathsFromAToB :: Network -> String -> String -> Int
getNumOfAllPathsFromAToB network nodeA nodeB = fst $ go M.empty nodeA
    where go seen node
            | node == nodeB = (1, seen)
            | node `M.member` seen = (seen M.! node, seen)
            | otherwise = (steps, M.insert node steps seen')
            where helper n (c,acc) = let (x,acc') = go acc n in (c+x,acc')
                  (steps,seen') = foldr helper (0,seen) $ M.findWithDefault [] node network

