module Main where

import Control.Arrow 
import Utils
import Data.Set (Set, fromList, member)
import Data.List (nub, groupBy, sortOn)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap 

parse :: [String] -> (Int, [Set Int])
parse (x:xs) = (head $ coordsFrom x, map (fromList . coordsFrom) xs)
  where 
    coordsFrom = map fst . filter ((/='.') . snd) . zip [0..]

solve1 (s, mirrors) = fst $ foldl update (0, [s]) mirrors
  where 
      update (count, positions) mirror = 
          let keep = [p | p <- positions, not (p `member` mirror)]
              split = [ [p - 1, p + 1] | p <- positions, p `member` mirror ]
          in (count + length split, nub (keep ++ concat split))

solve2 (s, mirrors) = countAlternatePaths (IntMap.singleton s 1) mirrors
  where 
    countAlternatePaths positions [] = sum $ map snd $ IntMap.toList positions
    countAlternatePaths positions (m:ms) = 
        let keep = [(p, mult) | (p, mult) <- IntMap.toList positions, not (p `member` m)]
            split = [ [(p - 1, mult), (p + 1, mult)] | (p, mult) <- IntMap.toList positions, p `member` m ]
            newPositions = IntMap.unionWith (+) (IntMap.fromList keep) (IntMap.fromListWith (+) (concat split))
        in countAlternatePaths newPositions ms

main :: IO ()
main = (solve1 &&& solve2) . parse . lines <$> readFile "inputs/2025/input07.txt"
         >>= print
