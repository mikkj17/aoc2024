import Data.List (sort)
import Data.List.Split (splitOn)

toTuple :: [Int] -> (Int, Int)
toTuple [x, y] = (x, y)

parse :: String -> ([Int], [Int])
parse inp = unzip $ map (toTuple . map read . splitOn "   ") (lines inp)

first :: String -> Int
first inp =
  let (left, right) = parse inp
   in sum $ zipWith (\a b -> abs $ a - b) (sort left) (sort right)

second :: String -> Int
second inp =
  let (left, right) = parse inp
   in sum $ map (\x -> x * length (filter (== x) right)) left

main :: IO ()
main = do
  testInput <- readFile "../../src/main/resources/day01/test-input.txt"
  input <- readFile "../../src/main/resources/day01/input.txt"
  print $ first input
  print $ second input
