{-# language MultiWayIf #-}
module Main where

import qualified Data.Set as S
import Data.List ( span )
import Data.List.Split ( splitOn )
import Data.Bifunctor ( bimap )

parse :: String -> [(Int, Int)]
parse = map (bimap read (read . tail) . span (/=',')) . splitOn " -> "

segToSet :: [(Int, Int)] -> S.Set (Int, Int)
segToSet xs = S.fromList $ concat $ zipWith createSeg xs $ tail xs
  where
    createSeg (a, b) (c, d) 
      | c > a || d > b = [(x,y) | x <- [a .. c], y <- [b .. d]]
      | otherwise      = createSeg (c, d) (a, b)

dropSand :: (Int -> Bool) -> ((Int, Int) -> S.Set (Int, Int) -> S.Set (Int, Int)) -> S.Set (Int, Int) -> S.Set (Int, Int)
dropSand p f blocks = go (500, 0)
  where
    blocked = (`S.member` blocks)
    free    = not . blocked
    go (x, y)
      | p y              = f (x, y) blocks
      | blocked (x, y+1) = if | free (x-1, y+1) -> go (x-1, y+1)
                              | free (x+1, y+1) -> go (x+1, y+1)
                              | otherwise       -> S.insert (x, y) blocks
      | otherwise = go (x, y+1)

solve :: (Int -> Bool) -> ((Int, Int) -> S.Set (Int, Int) -> S.Set (Int, Int)) -> (S.Set (Int, Int) -> S.Set (Int, Int) -> Bool) -> S.Set (Int, Int) -> Int
solve p f stop blocks = go blocks 0
  where
    go bs n
      | stop bs bs' = n
      | otherwise   = go bs' (n+1)
      where bs' = dropSand p f bs

main :: IO ()
main = do
  content <- map parse . lines <$> readFile "inputs/2022/input14.txt"
  let blocks = S.unions $ map segToSet content
      maxY   = S.findMax $ S.map snd blocks
  print $ solve (>maxY) (\_ b -> b) (==) blocks
  print $ solve (== maxY + 1) S.insert (\b _ -> (500, 0) `S.member` b) blocks -- part2 maxY blocks
