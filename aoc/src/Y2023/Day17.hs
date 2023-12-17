{-# LANGUAGE  TupleSections #-}
module Main ( main ) where 

import Rec
import Utils ( toCoord )
import qualified Data.Array as Array
import Control.Arrow ( (&&&) ) 
import Data.List ( sortOn )
import Data.Char ( digitToInt )
import qualified Data.Set as Set
import qualified Data.OrdPSQ as P

type Coord = (Int, Int)

toArr xs = Array.array ((0, 0), maxB) xs
  where maxB = maximum $ map fst xs 

s0 :: [(Coord, Coord, Int)]
s0 = [((0, 0), east, 0), ((0, 0), south, 0)]

east  = (0, 1)
south = (1, 0)

rot90, rotm90 :: (Int, Int) -> (Int, Int)
rot90 (x, y)      = (-y, x)
{-# INLINE rot90 #-}
rotm90 (x, y)     = (y, -x)
{-# INLINE rotm90 #-}

(a, b) .+. (c, d) = (a + c, b + d)
{-# INLINE (.+.) #-}

type Cand = ((Coord, Coord, Int), Int)

solve minStp maxStp desert = hylo alg coalg $ (, Set.empty) $ P.fromList $ map (\c -> (c,0,c)) s0
  where
    alg (Value x)    = x
    alg (Delayed xs) = xs

    coalg (pq, seen) = 
        case P.minView pq of 
          Nothing -> Value 0
          Just (x, c, _, xs) -> let next = filter ((`hasNotSeen` seen)) $ expand (x, c)
                                 in case filter isGoal next of 
                                      []    -> Delayed $ (merge xs next, insert x seen)
                                      (y:_) -> Value $ snd y

    (begin, end)              = Array.bounds desert
    isGoal ((coord, _, t), _) = coord == end && t >= minStp && t < maxStp
    inBound coord             = fst coord >= fst begin && snd coord >= snd begin && fst coord <= fst end && snd coord <= snd end
    insert c seen             = Set.insert c seen
    hasNotSeen c seen         = fst c `Set.notMember` seen
    getCost ix                = desert Array.! ix
    merge ys zs               = foldr (\(k, p) acc -> snd (P.alter (replace p k) k acc)) ys zs
    replace p k Nothing       = (p, Just (p, k))
    replace p k (Just (p', v)) = (p, if p < p' then Just (p, v) else Just (p', v))

    expand ((cur, dir, t), c) = [ (k, c + getCost nxt)  | let nxt = cur .+. dir
                                                        , inBound nxt, t < maxStp
                                                        , let k = (nxt, dir, t+1)
                                ]
                             <> [ (k, c + getCost nxt)  | let dir' = rot90 dir
                                                        , let nxt = cur .+. dir'
                                                        , inBound nxt, t  >= minStp 
                                                        , let k = (nxt, dir', 1)
                                ]
                             <> [ (k, c + getCost nxt)  | let dir' = rotm90 dir
                                                        , let nxt = cur .+. dir'
                                                        , inBound nxt, t  >= minStp 
                                                        , let k = (nxt, dir', 1)
                                ]

main :: IO ()
main = (solve 0 3 &&& solve 4 10) . toArr . map (\(c,x) -> (c, digitToInt x)) . toCoord . lines <$> readFile "inputs/2023/input17.txt"
         >>= print
