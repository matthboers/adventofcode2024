{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day2 where

day2_part1 :: IO ()
day2_part1 = do
    result <- readDay2
    let safeRows = map safe result
    print $ sum $ map fromEnum safeRows


safe :: (Ord a, Num a) => [a] -> Bool
safe xs = (allIncreasing || allDecreasing) && allDiffersByThree
    where zipped = zip xs (tail xs)
          allDecreasing = all (uncurry (<)) zipped
          allIncreasing = all (uncurry (>)) zipped
          allDiffersByThree = all (\(x,y) -> inRange (abs (x - y)) (1, 3)) zipped

inRange :: Ord a => a -> (a, a) -> Bool
inRange x (low, high) = x >= low && x <= high

readDay2 :: IO [[Int]]
readDay2 = do
    input <- readFile "input/day2-actual.txt"
    let colrows = map words $ lines input
    return $ map (map (\x -> read x :: Int)) colrows