module Main ( main ) where 

import qualified Data.Map.Strict as Map 
import Control.Arrow ( (&&&) )
import Utils ( toCoord )
import Rec
import Debug.Trace ( traceShow )

parser = cata alg . fromList . toCoord . lines
  where
    alg NilF = []
    alg (ConsF (xy, c) xs)
      | c == '#'  = xy : xs 
      | otherwise = xs

countOn f                    = Map.fromListWith (+) . (`zip` (repeat 1)) . map f
addTuple (a, b, _) (c, d, _) = (a+c, b+d)

sum' = cata alg 
  where 
    alg NilF = 0
    alg (ConsF (_, v) xs) = xs + v 

solve xs = addTuple (histo (alg totRows) rows) (histo (alg totCols) cols)
  where 
    rows    = fromList . Map.toAscList $ countOn fst xs
    cols    = fromList . Map.toAscList $ countOn snd xs
    totRows = sum' rows
    totCols = sum' cols

    alg tot NilF = (0, 0, 0)
    alg tot (ConsF (k, v) table) = 
        ( p1 + delta * (tot - delta) * (distance * 2 + 1)
        , p2 + delta * (tot - delta) * (distance * 1000000 + 1)
        , delta + v
        )
      where 
        (p1, p2, delta) = extract table
        distance = case nextElem table of
                     Nothing     -> 0
                     Just (h, _) -> (h - k - 1)


main :: IO ()
main = solve . parser <$> readFile "inputs/2023/input11.txt"
         >>= print
