module Y2015.Day03 (solution) where

import qualified Data.Set as S

makeMove :: (Int, Int) -> Char -> (Int, Int)
makeMove (x, y) '>' = (x+1, y)
makeMove (x, y) '<' = (x-1, y)
makeMove (x, y) '^' = (x, y+1)
makeMove (x, y) 'v' = (x, y-1)
makeMove _ _ = error "invalid char"

makeMoveAndSwap :: [(Int, Int)] -> Char -> [(Int, Int)]
makeMoveAndSwap [santa, other] m = [other, makeMove santa m]
makeMoveAndSwap _ _ = error "wrong list"

solution :: IO ()
solution = do
  content <- readFile "inputs/2015/input03.txt"
  let
    santa0 = (0,0)
    part1 = S.size . S.fromList . scanl makeMove santa0
    part2 = S.size . S.fromList . mconcat . scanl makeMoveAndSwap [santa0, santa0]
  print $ part1 content
  print $ part2 content
