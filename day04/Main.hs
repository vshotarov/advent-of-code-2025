module Main where

import qualified Common
import qualified Data.Set as S

main :: IO ()
main = do
    putStrLn $ "-- Solving day04 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = S.size $ getRemovableRolls parsedInput
    let answer2 = (S.size parsedInput-) . S.size . last
                . Common.takeUntil (S.null . getRemovableRolls)
                $ iterate (\s -> S.difference s $ getRemovableRolls s) parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Vec = (Int,Int)
type State = S.Set Vec

parse :: String -> State
parse input = S.fromList $ concat [[(x,y) | (x,c) <- zip [0..] l, c == '@']
                                   | (y,l) <- zip [0..] $ lines input]

getRemovableRolls :: State -> S.Set Vec
getRemovableRolls state =
    S.filter ((<4) . length . neighbours) state
    where neighbours (x,y) = [(nx,ny) | nx <- [x-1..x+1], ny <- [y-1..y+1],
                                        (nx,ny) `S.member` state
                                     && (x,y) /= (nx,ny)]

