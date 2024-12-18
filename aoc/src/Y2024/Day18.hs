module Main where

import Utils
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.IntMap ( IntMap )
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Debug.Trace

type Coord = (Int, Int)

parse :: String -> Coord
parse xs = let (x, y) = span (/=',') xs
             in  (read x, read (tail y))

up (x, y)    = (x, y-1)
down (x, y)  = (x, y+1)
left (x, y)  = (x-1, y)
right (x, y) = (x+1, y)

start = (0, 0)
end   = (70, 70)

inBound (x, y) = x >= fst start && x <= fst end && y >= snd start && y <= snd end

solve1 n walls' = go Set.empty (IntMap.singleton 0 (Set.singleton start))
  where
    walls = Set.fromList $ take n walls'
    go :: Set.Set Coord -> IntMap.IntMap (Set.Set Coord) -> Maybe Int
    go seen q
      | any (==end) positions = Just cost
      | Set.null sts          = Nothing
      | otherwise             = go seen' $ IntMap.unionWith (Set.union) q' $ IntMap.singleton (cost+1) sts
      where
        ((cost, positions), q') = IntMap.deleteFindMin q
        seen'  = Set.union seen positions
        sts    = Set.fromList
                 [ st           | pos <- Set.toList positions
                                , f <- [up, down, left, right]
                                , let st = f pos
                                , st `Set.notMember` seen
                                , inBound st
                                , st `Set.notMember` walls
                 ]
solve2 walls = let ix = go 1024 3450
               in walls !! (ix-1)
  where
    go l u | l > u = l
    go l u = let m = (l + u) `div` 2
                 cost = solve1 m walls
             in case cost of
                  Nothing -> if m == l then l else min m $ go l m
                  Just _  -> if m == l then u else go m u

f &&& g = \x -> (f x, g x)
solve = solve1 1024 &&& solve2

main :: IO ()
main = solve . map parse . lines <$> readFile "inputs/2024/input18.txt"
         >>= print
