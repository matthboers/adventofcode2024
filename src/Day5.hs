module Day5 where
import Data.Maybe
import Data.List

day5part1 :: IO ()
day5part1 = do
    input <- readDay5
    undefined


readDay5 = do
    text <- lines <$> readFile "input/day5-test.txt"
    let (orderingStr, updatesStr) = splitOnStr '|' text
    let orderings = map (splitOnStr '|') orderingStr
    let updates = map (splitOnStr ',') updatesStr
    print orderings
    print updates
    

splitOnStr :: Char -> [String] -> ([String], [String])
splitOnStr c s = (l, drop 1 r)
    where (l, r) = span (isJust . find (== c)) s