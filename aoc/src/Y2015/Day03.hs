module Y2015.Day03 (solution) where

import qualified Data.Map.Strict as M

makeMove :: Char -> (Int, Int) -> (Int, Int)
makeMove '>' (x, y) = (x+1, y)
makeMove '<' (x, y) = (x-1, y)
makeMove '^' (x, y) = (x, y+1)
makeMove 'v' (x, y) = (x, y-1)
makeMove _ _ = error "invalid char"

solution :: IO ()
solution = do
  content <- readFile "inputs/2015/input03.txt"
  let
    santa0 = ((0,0), 1)
    part1 = M.size . M.fromListWith (+) . scanl (\(pos, _) m -> (makeMove m pos, 1)) santa0
    part2 = M.size . M.fromListWith (+) . mconcat . scanl (\[(pos, _), santa] m -> [santa, (makeMove m pos, 1)]) [santa0, santa0]
  print $ part1 content
  print $ part2 content
