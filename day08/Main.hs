module Main where

import qualified Common
import Data.List.Split (splitOn)
import Data.List (sortOn, sort)
import qualified Data.Set as S

main :: IO ()
main = do
    putStrLn $ "-- Solving day08 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let states = scanl (\(_,acc) x -> (x,mergeJunctions acc x))
                       (((0,0,0),(0,0,0)), (map S.singleton parsedInput))
                       closest
            where closest = map fst . sortOn snd
                          $ [((a,b),sqDistance a b) | (a,b) <- Common.pairs parsedInput]

    let answer1 = product . take 3 . reverse . sort . map S.size . snd . last
                $ take (n+1) states
            where n = if length parsedInput < 999 then 10 else 1000
    let answer2 = (\(((ax,_,_),(bx,_,_)),_) -> ax*bx)
                . last $ Common.takeUntil (\(_,s) -> length s == 1) states

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Vec = (Int,Int,Int)

parse :: String -> [Vec]
parse input = map parseOne $ lines input
    where parseOne l = case splitOn "," l of
                         [a,b,c] -> (read a, read b, read c)
                         _ -> error "invalid input"

sqDistance :: Vec -> Vec -> Int
sqDistance (a,b,c) (i,j,k) = dx*dx + dy*dy + dz*dz
    where (dx,dy,dz) = (i-a,j-b,k-c)

mergeJunctions :: [S.Set Vec] -> (Vec,Vec) -> [S.Set Vec]
mergeJunctions circuits (a,b) = merged:removedABCircuits
    where abCircuits = filter (\c -> a `S.member` c || b `S.member` c) circuits
          removedABCircuits = filter (not . (`elem` abCircuits)) circuits
          merged = foldr1 S.union abCircuits

