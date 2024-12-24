{-# LANGUAGE FlexibleContexts #-}

module Day3 where
import Text.Regex.TDFA ( (=~), AllTextMatches(getAllTextMatches) )

part1Regex :: String
part2Regex :: String
part1Regex = "mul\\(([[:digit:]]+),([[:digit:]]+)\\)"
part2Regex = "mul\\(([[:digit:]]+),([[:digit:]]+)\\)|do\\(\\)|don't\\(\\)"

day3part1 :: IO ()
day3part1 = do
    input <- readDay3
    let parsed = getNumbers $ parseDay3 part1Regex input
    print $ sum $ map (uncurry (*)) parsed

day3part2 :: IO ()
day3part2 = do
    input <- readDay3
    let parsed = getNumbers $ filterOnDoAndDont True $ parseDay3 part2Regex input
    print $ sum $ map (uncurry (*)) parsed

readDay3 :: IO String
readDay3 = readFile "input/day3-actual.txt"

parseDay3 :: String -> String -> [String]
parseDay3 rx s = getAllTextMatches (s =~ rx) :: [String]

getNumbers :: [String] -> [(Int, Int)]
getNumbers = map (mapToPairs . mapToAttr)
    where mapToAttr s = s =~ part1Regex :: (String, String, String, [String])
          mapToPairs (_, _, _, [x, y]) = (read x, read y)
          mapToPairs (_, _, _, _) = undefined

filterOnDoAndDont :: Bool -> [String] -> [String]
filterOnDoAndDont _     []             = []
filterOnDoAndDont _     ("do()":xs)    = filterOnDoAndDont True xs
filterOnDoAndDont _     ("don't()":xs) = filterOnDoAndDont False xs
filterOnDoAndDont True  (x:xs)         = x : filterOnDoAndDont True xs
filterOnDoAndDont False (_:xs)         = filterOnDoAndDont False xs