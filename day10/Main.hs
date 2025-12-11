module Main where

import qualified Common
import Data.List.Split (splitOn)
import qualified Data.Set as S

main :: IO ()
main = do
    putStrLn $ "-- Solving day10 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = sum $ map (uncurry getButtonPressesToAchieveLights . (\(l,b,_) -> (l,b))) parsedInput
    let answer2 = "Unfortunately only solved in python using z3. Refer to day10/part2.py"

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> [([Int],[[Int]],[Int])]
parse input = map parseLine $ lines input
    where parseLine l = let (lights,rest) = Common.splitOnceOn " " l
                            (buttons,joltage) = Common.splitOnceOn "{" rest
                            lights' = map (fromEnum . (=='#')) . init $ tail lights
                            buttons' = map (map read . splitOn "," . init . tail) $ words buttons :: [[Int]]
                            joltage' = map read . splitOn "," $ init joltage :: [Int]
                         in (lights',buttons',joltage')

getButtonPressesToAchieveLights :: [Int] -> [[Int]] -> Int
getButtonPressesToAchieveLights lights buttons =
    go S.empty [(take (length lights) $ repeat 0, 0)]
    where go _ [] = error "could not find combination"
          go seen ((ls,b):queue)
            | ls `S.member` seen = go seen queue
            | ls == lights = b
            | otherwise = go (S.insert ls seen) $ queue ++ nextSteps
            where toggle ls' bs' = [if i `elem` bs' then (1-l) else l
                                   | (i,l) <- zip [0..] ls']
                  nextSteps = map (\but -> (toggle ls but, b+1)) buttons

