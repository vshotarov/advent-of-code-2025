module Main where

import qualified Common
import qualified Data.Set as S
import qualified Data.Map as M

main :: IO ()
main = do
    putStrLn $ "-- Solving day07 --"
    input <- Common.readInput

    -- Parse
    let parsedInput@(splitters, maxY) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let start = head . filter ((==0) . snd) $ S.toList splitters
    let (answer1,answer2) = solve maxY (fst start) splitters

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Vec = (Int,Int)

parse :: String -> (S.Set Vec, Int)
parse input = (S.fromList . concatMap parseLine $ zip [0..] inputLines,
               length inputLines-1)
    where inputLines = lines input
          parseLine (y,l) = [(x,y) | (x,c) <- zip [0..] l, c `elem` "S^"]

solve :: Int -> Int -> S.Set Vec -> (Int,Int)
solve maxY startX splitters = (answer1, sum $ M.elems finalState)
    where splittersAtY y = S.map fst $ S.filter ((==y) . snd) splitters
          split x s = M.insertWith (+) (x-1) c . M.insertWith (+) (x+1) c
                    $ M.delete x s
            where c = s M.! x
          helper x (c,s)
            | x `M.member` s = (c+1, split x s)
            | otherwise = (c,s)
          startState = (0, M.fromList [(startX, 1)])
          (answer1,finalState) = foldl (\acc y -> foldr helper acc $ splittersAtY y)
                                       startState [1..maxY]

