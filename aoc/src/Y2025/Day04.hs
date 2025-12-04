module Main ( main ) where

import Utils
import Data.List ( transpose, sort )
import qualified Data.Set as S
import Control.Arrow 

getRemovable :: S.Set (Int, Int) -> [(Int, Int)]
getRemovable s = [pos | pos <- S.toList s, let n = filter (`S.member` s) (neighbors pos), length n < 4]
  where 
    neighbors (x, y) = [(x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0)]

solve1 :: S.Set (Int, Int) -> Int
solve1 = length . getRemovable

solve2 :: S.Set (Int, Int) -> Int
solve2 s 
  | null toRemove = 0
  | otherwise = length toRemove + solve2 (s S.\\ S.fromList toRemove) 
  where
    toRemove = getRemovable s 

main :: IO ()
main = (solve1 &&& solve2) . S.fromList . map fst . filter (\(_,x) -> x=='@') . toCoord . lines <$> readFile "inputs/2025/input04.txt"
         >>= print
