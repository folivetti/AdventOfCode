module Y2022.Day04 ( solution ) where

import Data.List.Split ( splitOn )
import Control.Arrow ( (&&&) )

type Pair a    = (a, a)
type PairsOf a = Pair (Pair a)

parse :: String -> PairsOf Int
parse = sortInterval . map (map read . splitOn "-") . splitOn ","
  where 
      sortInterval [[x,y], [a,b]] 
          | a < x            = ((a,b), (x,y))
          | x == a && b >= y = ((a,b), (x,y))
          | otherwise        = ((x,y), (a,b))
      sortInterval _ = error "wrong!"

doesCover, doesOverlap :: PairsOf Int -> Bool
doesCover ((_,y), (_,b))   = y >= b
doesOverlap ((_,y), (a,_)) = y >= a

solve :: (PairsOf Int -> Bool) -> [PairsOf Int] -> Int
solve f = length . filter f

solution :: IO ()
solution = do content <- map parse . lines <$> readFile "inputs/2022/input04.txt"
              print $ (solve doesCover &&& solve doesOverlap) content
