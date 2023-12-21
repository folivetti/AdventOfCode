module Main ( main ) where 

import Utils ( toCoord )
import Rec
import Data.Map.Strict ( Map )
import Data.Set ( Set ) 
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Arrow ( (&&&) ) 

getS0 = Set.singleton . fst . head . Map.toList . Map.filter (=='S')
lenSqr = floor . sqrt . fromIntegral . Map.size 

--bfs :: (a -> Set a -> Set a) -> Set a -> [Int]
--bfs step states = let newStates = Set.foldr step Set.empty states
--                   in Set.size newStates : bfs step newStates

bfs step f = toList . ana coalg
  where
    coalg states = let newStates = Set.foldr step Set.empty states 
                    in ConsF (f newStates) newStates 

solve1 arr = bfs step Set.size (getS0 arr) !! 63
  where 
    step (x, y) seen = Set.union seen $ Set.fromList
                       [ (z, w) | (z, w) <- [(x+1, y), (x, y+1), (x-1, y), (x, y-1)]
                                , (z, w) `Map.member` arr
                                , arr Map.! (z, w) /= '#'
                       ]

solve2 arr = interpolate (ys !! r, ys !! (r + width), ys !! (r + 2*width))
  where 
    ys    = bfs step Set.size (getS0 arr)
    width = lenSqr arr

    (steps, r) = 26501365 `quotRem` width

    interpolate (y1,y2,y3) = y1 + steps * (y2-y1) + (steps * (steps - 1) `div` 2) * ((y3-y2) - (y2 - y1))

    wrap z len = if z < 0 
                   then (len + (z `rem` len)) `rem` len 
                   else z `rem` len 

    step (x, y) seen = Set.union seen $ Set.fromList
                       [ (z, w) | (z, w) <- [(x+1, y), (x, y+1), (x-1, y), (x, y-1)]
                                , let (z', w') = (wrap z width, wrap w width)
                                , arr Map.! (z', w') /= '#'
                       ]

main :: IO ()
main = do (solve1 &&& solve2) . Map.fromList . toCoord . lines <$> readFile "inputs/2023/input21.txt"
           >>= print
