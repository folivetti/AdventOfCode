{-# LANGUAGE  DeriveFunctor #-}
module Main ( main ) where 

import Rec
import Utils ( toCoord )
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Control.Arrow ( (&&&) ) 
import Control.Parallel.Strategies ( parMap, rpar )

data Coord = C Int Int deriving (Show, Eq, Ord)

instance Num Coord where 
    (C x y) + (C z w) = C (x+z) (y+w)
    (C x y) - (C z w) = C (x-z) (y-w)
    (C x y) * (C z w) = C (x*z) (y*w)
    abs (C x y)       = C (abs x) (abs y)
    signum (C x y)    = 0
    fromInteger x     = C (fromInteger x) (fromInteger x)
    negate (C x y)    = C (negate x) (negate y)
    
toC (x, y)   = C x y
getX (C x _) = x 
getY (C _ y) = y
swap (C x y) = C y x

s0s = concat [ [(C 0 x, C 1 0), (C x 0, C 0 1), (C 109 x, C (-1) 0), (C x 109, C 0 (-1))] | x <- [0 .. 109] ]

solve s0 contraption = Set.size $ hylo alg coalg ([s0], Set.singleton s0)
  where
    alg NilF           = Set.empty
    alg (ConsF c cs)   = Set.fromList c <> cs

    coalg ([], _) = NilF 
    coalg (candidates, seen) = let candidates' = filter ((`Map.member` contraption) . fst) . filter (not . (`Set.member` seen)) $ concatMap step candidates
                                   seen'       = Set.fromList candidates' <> seen
                                in ConsF (map fst candidates) (candidates', seen')

    step (coord, dir) =
        case contraption Map.!? coord of 
          Nothing -> []
          Just '|' | getX dir == 0 -> [(coord + C (-1) 0, C (-1) 0), (coord + C 1 0, C 1 0)]
          Just '-' | getY dir == 0 -> [(coord + C 0 (-1), C 0 (-1)), (coord + C 0 1, C 0 1)]
          Just '/' -> let dir' = negate (swap dir) in [(coord + dir', dir')]
          Just '\\' -> let dir' = swap dir in [(coord + dir', dir')]
          _ -> [(coord + dir, dir)]


solve1    = solve (C 0 0, C 0 1)
solve2 xs = maximum $ parMap rpar (`solve` xs) s0s

main :: IO ()
main = (solve1 &&& solve2) . Map.fromList . map (\(c,x) -> (toC c, x)) . toCoord . lines <$> readFile "inputs/2023/input16.txt"
         >>= print
