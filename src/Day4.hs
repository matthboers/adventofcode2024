module Day4 where
import Data.List (transpose)
import Text.Regex.TDFA (AllTextMatches(getAllTextMatches), (=~))
import Data.Maybe (catMaybes)

day4part1 :: IO ()
day4part1 = do
    input <- readDay4
    print $ sum $ map sum (iterateOverAllCombinations (allCombinations input))

allCombinations :: [String] -> [[String]]
allCombinations s = [rows, columns, diagonalsR, diagonalsL]
    where rows = s
          columns = transpose s
          diagonalsR = diagonals s
          diagonalsL = diagonals $ map reverse s

iterateOverAllCombinations :: [[String]] -> [[Int]]
iterateOverAllCombinations = map (map findMatches)

diagonals :: [[a]] -> [[a]]
diagonals xs = map catMaybes $ transpose $ map addPadding $ calculatePadding xs
    where calculatePadding ys = zipWith (\x i -> (x, i, length ys - i - 1)) xs [0..]
          addPadding (ys, padRight, padLeft) = replicate padLeft Nothing ++ map Just ys ++ replicate padRight Nothing

findMatches :: String -> Int
findMatches s = length $ 
                (getAllTextMatches (s =~ "XMAS") :: [String]) ++ 
                (getAllTextMatches (s =~ "SAMX") :: [String])

readDay4 :: IO [String]
readDay4 = do
    text <- readFile "input/day4-actual.txt"
    return $ lines text