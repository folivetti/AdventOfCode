module Main ( main ) where 

import Utils ( toCoord )
import Rec
import qualified Data.Array as Array 
import qualified Data.Map.Strict as Map 
import qualified Data.Set as Set
import Data.Bifunctor ( second )
import Data.List ( maximum )
import Control.Parallel.Strategies ( parMap, rpar )

type Coord = (Int, Int)
type Forest = Array.Array Coord Char 

toArr xs = let end = fst (maximum xs)
            in Array.array ((0, 0), end) xs

nextEdge s0 goal step  = hylo delayed coalg -- (visited, froms, g)
  where
    isJunction x | x == s0 || x == goal = True
                 | otherwise            = length (step x Set.empty) > 2

    coalg (_, [], g) = Value g 
    coalg (visited, (x,d):xs, g)
      | d > 0 && isJunction x = Delayed (visited, xs, g')
      | otherwise             = Delayed (visited', xs', g)
      where 
        neighs   = step x visited
        visited' = foldr Set.insert visited $ filter (not . isJunction) neighs
        xs'      = xs <> zip neighs (repeat (d+1))
        g' | x `Map.member` g = Map.adjust (max d) x g 
           | otherwise        = Map.insert x d g 

dfs step goal s0 =  largestPath goal s0 $ hylo delayed coalg (Set.singleton s0, Map.empty, [s0])
  where
    coalg (_, g, [])         = Value g
    coalg (visited, g, x:xs) =
        let edges = Map.assocs $ nextEdge s0 goal step (Set.singleton x, [(x, 0)], Map.empty)
            g'    = Map.insert x edges g
            vis   = foldr Set.insert visited nxt
            nxt   = filter (`Set.notMember` visited) $ map fst edges
         in Delayed (vis, g', xs <> nxt)

largestPath goal s0 g = go s0 Set.empty 
  where
    go s seen 
      | s == goal = Just 0
      | s `Set.member` seen = Nothing
      | s `Map.notMember` g = Nothing
      | otherwise = let xs = parMap rpar (\(c, d) -> fmap (d+) (go c (Set.insert s seen))) (g Map.! s)
                            -- [fmap (+d) (go c (Set.insert s seen)) | (c, d) <- g Map.! s]
                     in if null xs then Nothing else maximum xs

hike :: Array.Array (Int, Int) Char -> (Array.Array (Int, Int) Char -> (Int, Int) -> (Int, Int) -> Bool) -> (Int, Int) -> Set.Set (Int, Int) -> [(Int, Int)]
hike forest f (x, y) visited = 
    [z | z <- [(x,y+1), (x+1,y), (x,y-1), (x-1,y)]
       , inBound z forest
       , forest Array.! z /= '#'
       , f forest (x, y) z
       , z `Set.notMember` visited
    ]

slip forest (x, y) (a, b) =
    case forest Array.! (x, y) of 
      '>' -> (x, y+1) == (a, b)
      '<' -> (x, y-1) == (a, b)
      '^' -> (x-1, y) == (a, b)
      'v' -> (x+1, y) == (a, b)
      '#' -> False
      _   -> True

inBound (x, y) arr = let ((a, b), (c, d)) = Array.bounds arr 
                      in x >= a && x <= c && y >= b && y <= d

solve forest = (part1, part2)
  where
    s0   = (0, 1)
    goal = second (subtract 1) . snd $ Array.bounds forest
    part1 = dfs (hike forest slip) goal s0 
    part2 = dfs (hike forest (\_ _ _ -> True)) goal s0 

main :: IO ()
main = solve . toArr . toCoord . lines <$> readFile "inputs/2023/input23.txt"
          >>= print
