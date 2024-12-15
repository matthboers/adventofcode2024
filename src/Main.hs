module Main where
import Data.List

main :: IO ()
main = day1_part2

day1_part1 :: IO ()
day1_part1 = do
    (list1, list2) <- readDay1
    let sorted1 = sort list1
    let sorted2 = sort list2
    let zipped = zip sorted1 sorted2
    let distances = map (\(x,y) -> abs (x - y)) zipped
    let total = sum distances
    print total

day1_part2 :: IO ()
day1_part2 = do
    (list1, list2) <- readDay1
    let similarity = map (\x -> (length . filter (x==)) list2) list1
    let multiplied = zipWith (*) list1 similarity
    print $ sum multiplied


readDay1 :: IO ([Int], [Int])
readDay1 = do
    input <- readFile "input/day1-actual.txt"
    let colrows = map words $ lines input
    let rotated = transpose colrows
    let parsed = map (map (\x -> read x :: Int)) rotated
    return (head parsed, last parsed)