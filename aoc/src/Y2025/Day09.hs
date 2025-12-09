module Main where 

import Control.Arrow ((&&&))
import Data.List ( break )

parse :: String -> (Int, Int) 
parse = (\(a, b) -> (read a, read $ tail b)) . break (== ',')

intersects (x, y) (z, w) = not . all outside . pairs 
  where 
      pairs (p : ps) = zip (p : ps) (ps ++ [p])
      outside ((a, b), (c, d)) = (max a c <= min x z) || min a c >= max x z ||
                                 (max b d <= min y w) || min b d >= max y w

solve1 points = maximum [ (1 + abs (x-z)) * (1 + abs (y-w)) | (x, y) <- points, (z, w) <- points ]
solve2 points = maximum [ (1 + abs (x-z)) * (1 + abs (y-w)) | (x, y) <- points, (z, w) <- points,
                          not $ intersects (x, y) (z, w) points]

main :: IO ()
main = (solve1 &&& solve2) . map parse . lines <$> readFile "inputs/2025/input09.txt"
         >>= print
