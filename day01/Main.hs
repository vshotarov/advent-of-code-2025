module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day01 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = length . filter (==0) $ scanl (\acc x -> (acc + x) `mod` 100) 50 parsedInput
    let answer2 = solve2 parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> [Int]
parse = map parseLine . lines
    where parseLine [] = error "bad input"
          parseLine [_] = error "bad input"
          parseLine ('L':xs) = - (read xs)
          parseLine ('R':xs) = read xs
          parseLine _ = error "bad input"

solve2 :: [Int] -> Int
solve2 instructions = go 0 50 instructions
    where go z _ [] = z
          go z c (i:ins) = go z' c' ins
              where -- number of full revolutions regardless of state
                    r = (abs i) `div` 100
                    -- signed number of actual clicks difference (i.e. i - r * 100)
                    i' = ((abs i) `mod` 100) * (if i < 0 then (-1) else 1)
                    -- adding that click difference to the current state
                    cpi = c + i'
                    -- the next state is that % 100
                    c' = cpi `mod` 100
                    -- if the new state is above 100 or at 0 then we add 1
                    -- extra revolution to the full revolutions from before,
                    -- since we are seeing a 0
                    r' = r + if c' /= cpi || c' == 0 then 1 else 0
                    -- we only apply that addition of the extra revolution if
                    -- we are not already at 0, as if we are that's already handled
                    z' = z + if c == 0 then r else r'

