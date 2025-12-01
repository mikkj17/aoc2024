import Data.Maybe (mapMaybe)

type Grid = [[Char]]

type Position = (Int, Int)

type Direction = (Int, Int)

directions :: [Direction]
directions =
  [ (-1, -1),
    (-1, 0),
    (-1, 1),
    (0, -1),
    (0, 1),
    (1, -1),
    (1, 0),
    (1, 1)
  ]

getAtPosition :: Grid -> Position -> Maybe Char
getAtPosition grid (y, x)
  | 0 < y || length grid <= y = Nothing
  | 0 < x || length (head grid) <= x = Nothing
  | otherwise = Just ((grid !! y) !! x)

generateDiffs :: Int -> [Int]
generateDiffs i = map (* i) [0 .. 3]

generateIndicesForOneAxis :: Int -> Int -> [Int]
generateIndicesForOneAxis i di = map (i -) (generateDiffs di)

generateIndices :: Position -> Direction -> [Position]
generateIndices (y, x) (yy, xx) = zip (generateIndicesForOneAxis y yy) (generateIndicesForOneAxis x xx)

generateWord :: Grid -> Position -> Direction -> String
generateWord grid pos dir = mapMaybe (getAtPosition grid) (generateIndices pos dir)

generateWords :: Grid -> Position -> [String]
generateWords grid position = map (generateWord grid position) directions

-- first :: String -> Int
first inp =
  let grid = lines inp
   in generateWords grid (3, 4)

second :: String -> Int
second inp = error "not implemented yet"

main :: IO ()
main = do
  testInput <- readFile "../../src/main/resources/day04/test-input.txt"
  input <- readFile "../../src/main/resources/day04/input.txt"
  print $ first testInput

-- print $ second testInput
