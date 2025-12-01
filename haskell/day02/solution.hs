import Data.List (sort, sortBy)
import Data.List.Split (splitOn)

parse :: String -> [[Int]]
parse inp = map (map read . splitOn " ") (lines inp)

safe :: [Int] -> Bool
safe xs =
  xs `elem` [sort xs, sortBy (flip compare) xs]
    && all
      (\x -> x `elem` [1 .. 3])
      (zipWith (\a b -> abs $ a - b) xs (tail xs))

first :: String -> Int
first inp = length $ filter safe (parse inp)

generate :: [Int] -> [[Int]]
generate xs = [take i xs ++ drop (i + 1) xs | i <- [0 .. (length xs)]]

second :: String -> Int
second inp = length $ filter (any safe . generate) (parse inp)

main :: IO ()
main = do
  testInput <- readFile "../../src/main/resources/day02/test-input.txt"
  input <- readFile "../../src/main/resources/day02/input.txt"
  print $ first input
  print $ second input
