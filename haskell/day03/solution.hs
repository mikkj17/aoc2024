import Text.Printf (printf)
import Text.Regex.Posix ((=~))

mulPattern :: String
mulPattern = "mul\\(([0-9]+),([0-9]+)\\)"

doPattern :: String
doPattern = "do\\(\\)"

dontPattern :: String
dontPattern = "don't\\(\\)"

combinedPattern :: String
combinedPattern = printf "%s|%s|%s" mulPattern doPattern dontPattern

-- Function to find the first match and extract groups
findFirstMatch :: String -> String -> (Int, Int)
findFirstMatch pattern text =
  let (_, _, _, [x, y]) = text =~ pattern :: (String, String, String, [String])
   in (read x, read y)

-- Function to find all matches and extract groups
findAllGroups :: String -> String -> [(Int, Int)]
findAllGroups pattern text =
  let matches = text =~ pattern :: [[String]]
   in map (\(_ : x : y : _) -> (read x, read y)) matches -- Extract groups x and y

-- Function to find all matches
findAllMatches :: String -> String -> [String]
findAllMatches pattern text =
  let matches = text =~ pattern :: [[String]]
   in map head matches -- Extract the full matches

first :: String -> Int
first inp =
  let groups = findAllGroups mulPattern inp
   in sum $ map (uncurry (*)) groups

parseInstruction :: (Int, Bool) -> String -> (Int, Bool)
parseInstruction (total, _) "do()" = (total, True)
parseInstruction (total, _) "don't()" = (total, False)
parseInstruction (total, enabled) instruction =
  if enabled
    then
      ( let (x, y) = findFirstMatch mulPattern instruction
         in (total + x * y, True)
      )
    else (total, False)

second :: String -> Int
second inp =
  let matches = findAllMatches combinedPattern inp
   in fst $ foldl parseInstruction (0, True) matches

main :: IO ()
main = do
  testInput <- readFile "../../src/main/resources/day03/test-input.txt"
  testInput2 <- readFile "../../src/main/resources/day03/test-input2.txt"
  input <- readFile "../../src/main/resources/day03/input.txt"
  print $ first input
  print $ second input
