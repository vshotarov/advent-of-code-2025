module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day09 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let pairs = Common.pairs parsedInput
    let answer1 = maximum $ map (uncurry area) pairs
    let answer2 = maximum . map (uncurry area . toPair)
                . filter (not . rectIntersectsShape) $ map toRect pairs
            where edges = zip parsedInput $ tail parsedInput
                  toPair (a,_,_,b) = (a,b)
                  lineIntersectsRect ((x1,y1),(x2,y2)) ((ax,ay),(bx,_),(cx,cy),_)
                    -- only works for the actual input, rather than the example, as the example
                    -- contains large possible rectangles OUTSIDE of the shape, while the input
                    -- is a circle with a cutout rectangle in it, so no large rectangles are
                    -- outside of the shape
                    | y1 == y2 && y1 > ay && y1 < cy = not ((max x1 x2 <= ax) || (min x1 x2) >= bx)
                    | x1 == x2 && x1 > ax && x1 < cx = not ((max y1 y2 <= ay) || (min y1 y2) >= cy)
                    | otherwise = False
                  rectIntersectsShape rect = any (\l -> lineIntersectsRect l rect) edges

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Vec = (Int,Int)
type Rect = (Vec,Vec,Vec,Vec)

parse :: String -> [Vec]
parse input = map (Common.mapTuple read . Common.splitOnceOn ",") $ lines input

area :: Vec -> Vec -> Int
area (x1,y1) (x2,y2) = (abs (x2-x1) + 1) * (abs (y2-y1) + 1)

toRect :: (Vec,Vec) -> Rect
toRect ((ax,ay),(bx,by)) = ((min ax bx, min ay by), (max ax bx, min ay by),
                            (min ax bx, max ay by), (max ax bx, max ay by))


