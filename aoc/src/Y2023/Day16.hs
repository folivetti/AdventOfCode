{-# LANGUAGE  DeriveFunctor #-}
module Main ( main ) where 

import Rec
import Utils ( toCoord )
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Control.Arrow ( (&&&) ) 
import Control.Parallel.Strategies ( parMap, rpar )
import Data.Semigroup ( Max(..) )

rot90, rotm90 :: (Int, Int) -> (Int, Int)
rot90 (x, y)      = (y, x)
{-# INLINE rot90 #-}
rotm90 (x, y)     = (-y, -x)
{-# INLINE rotm90 #-}
(.+.) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(x, y) .+. (a, b) = (x+a, y+b)
{-# INLINE (.+.) #-}

east, west, north, south :: (Int, Int)
east  = (0, 1)
west  = (0, -1)
north = (-1, 0)
south = (1, 0)

s0s :: [[((Int, Int), (Int, Int))]]
s0s = [ [((0, x), south), ((x, 0), east), ((109, x), north), ((x, 109), west)] | x <- [0 .. 109] ]

solve :: ((Int, Int), (Int, Int)) -> Map.Map (Int, Int) Char -> Int
solve s0 contraption = Set.size $ hylo alg coalg ([s0], Set.singleton s0)
  where
    alg NilF           = Set.empty
    alg (ConsF c cs)   = Set.union c cs

    coalg ([], _) = NilF 
    coalg (candidates, seen) = let candidates' = foldr (step (valid seen)) [] candidates
                                   seen'       = Set.fromList candidates' <> seen
                                in ConsF (Set.fromList $ map fst candidates) (candidates', seen')

    valid seen (c, d) = (c, d) `Set.notMember` seen && c `Map.member` contraption

    step p (coord, dir) acc =
        case contraption Map.!? coord of 
          Nothing -> acc
          Just '|' | fst dir == 0 -> [(coord .+. d, d) | d <- [north, south], p (coord .+. d, d)] <> acc 
          Just '-' | snd dir == 0 -> [(coord .+. d, d) | d <- [west, east], p (coord .+. d, d)] <> acc
          Just '/'                -> let dir' = rotm90 dir 
                                         pt   = (coord .+. dir', dir')
                                      in if p pt then pt : acc else acc 
          Just '\\'               -> let dir' = rot90 dir 
                                         pt   = (coord .+. dir', dir')
                                      in if p pt then pt : acc else acc
          _                       -> let pt = (coord .+. dir, dir) 
                                      in if p pt then pt : acc else acc

solve1    = solve ((0, 0), east)
solve2 xs = maximum $ parMap rpar (foldMap (Max . (`solve` xs))) s0s

main :: IO ()
main = (solve1 &&& solve2) . Map.fromList . toCoord . lines <$> readFile "inputs/2023/input16.txt"
         >>= print
