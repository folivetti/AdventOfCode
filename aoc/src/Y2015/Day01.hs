module Main where

char2num :: Char -> Int
char2num '(' = 1
char2num ')' = -1
char2num _ = error "wrong char in input"

cumsum :: [Int] -> [Int]
cumsum = scanl1 (+)

findFst :: Eq b => b -> [b] -> Int
findFst x = fst . head . dropWhile ((/= x) . snd) . zip [1..]

main :: IO ()
main = do
  content <- readFile "inputs/2015/input01.txt"
  let
    nums  = map char2num content
    part1 = sum nums
    part2 = findFst (-1) $ cumsum nums
  print part1
  print part2
