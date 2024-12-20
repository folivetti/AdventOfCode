{-# LANGUAGE TupleSections #-}
module Main where

import Utils
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.IntMap ( IntMap )
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.List
import Control.Monad

type Coord = (Int, Int)

parse :: [(Coord, Char)] -> (Coord, Coord, Set Coord)
parse grid = (start, end, walls)
  where
    start = fst . head . filter ((=='S').snd) $ grid
    end   = fst . head . filter ((=='E').snd) $ grid
    walls = Set.fromList . map fst . filter ((=='#').snd) $ grid

up (x, y)    = (x, y-1)
down (x, y)  = (x, y+1)
left (x, y)  = (x-1, y)
right (x, y) = (x+1, y)

manhattan (x, y) (a, b) = abs (x-a) + abs (y-b)

solve :: (Coord, Coord, Set.Set Coord) -> (Int, Int)
solve (start, end, walls) = (length $ filter (==2) cheats, length cheats)
  where
    cheats = do x:xs <- tails path
                y    <- xs
                let dist = manhattan (fst x) (fst y)
                guard $ dist <= 20
                      && abs (snd x - snd y) >= 100 + dist
                pure dist

    path = reverse $ go [(start, 0)]
    go [(x, c)] = go (head [(z,c+1) | f <- [up, down, left, right], let z = f x, z `Set.notMember` walls] : [(x,c)])
    go zs@((x,cx):(y,cy):xs)
      | x == end  = zs
      | otherwise = go (head [(z,cx+1) | f <- [up, down, left, right], let z = f x, z `Set.notMember` walls, z /= y] : zs)

main :: IO ()
main = solve . parse . toCoord . lines <$> readFile "inputs/2024/input20.txt"
         >>= print
