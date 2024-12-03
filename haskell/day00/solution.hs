first :: String -> Int
first inp = error "not implemented yet"

second :: String -> Int
second inp = error "not implemented yet"

main :: IO ()
main = do
  testInput <- readFile "../../src/main/resources/day00/test-input.txt"
  input <- readFile "../../src/main/resources/day00/input.txt"
  print $ first testInput
  print $ second testInput
