module Day4 where
import Data.List (transpose, unfoldr)
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

day4part2 :: IO ()
day4part2 = do
    input <- readDay4
    let blocks = concatMap generateBlocks (generateSets input)
    print $ sum $ map (fromEnum . matchBlock) blocks

generateSets :: [a] -> [[a]]
generateSets = unfoldr generate
    where generate ys
           | length ys >= 3 = Just (take 3 ys, drop 1 ys)
           | otherwise      = Nothing

generateBlocks :: [[a]] -> [[[a]]]
generateBlocks = unfoldr generate
    where generate yys
           | length (head yys) >= 3 = Just (map (take 3) yys, map (drop 1) yys)
           | otherwise            = Nothing


matchBlock :: [String] -> Bool
matchBlock [['M', _, 'S'], [_, 'A', _], ['M', _, 'S']] = True
matchBlock [['M', _, 'M'], [_, 'A', _], ['S', _, 'S']] = True
matchBlock [['S', _, 'M'], [_, 'A', _], ['S', _, 'M']] = True
matchBlock [['S', _, 'S'], [_, 'A', _], ['M', _, 'M']] = True
matchBlock _ = False

readDay4 :: IO [String]
readDay4 = do
    text <- readFile "input/day4-actual.txt"
    return $ lines text