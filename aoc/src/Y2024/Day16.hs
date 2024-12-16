module Main where

import Utils
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.IntMap ( IntMap )
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map

type Coord = (Int, Int)

parse :: [(Coord, Char)] -> (Coord, Coord, Set Coord)
parse grid = (start, end, walls)
  where
    start = fst . head . filter ((=='S').snd) $ grid
    end   = fst . head . filter ((=='E').snd) $ grid
    walls = Set.fromList . map fst . filter ((=='#').snd) $ grid

addCoord (x, y) (a, b) = (x+a, y+b)
rotateLeft (x, y)      = (-y, x)
rotateRight (x, y)     = (y, -x)

solve (start, end, walls) = go Set.empty (IntMap.singleton 0 q0)
  where
    d0 = (0, 1)
    q0 = Map.singleton (start, d0) (Set.singleton start)

    go seen q
      | Map.null ends = go seen' (IntMap.unionWith (Map.unionWith Set.union) q' sts)
      | otherwise     = (cost, Set.size (Set.unions $ Map.elems ends) )
      where
        ((cost, paths), q') = IntMap.deleteFindMin q
        -- paths' = Map.withoutKeys paths seen
        ends   = Map.filterWithKey (\k _ -> k `Set.notMember` seen && fst k == end) paths
        seen'  = Set.union seen $ Set.fromList $ Map.keys $ Map.filterWithKey (\k _ -> k `Set.notMember` seen) paths
        sts    = IntMap.fromListWith (Map.unionWith Set.union)
                 [ st | ((pos, dir), path') <- Map.toList paths
                      , (pos, dir) `Set.notMember` seen
                      , st <- [(cost + 1001, Map.singleton (pt, dir') path'') | let dir' = rotateRight dir, let pt = (addCoord pos dir'), (pt, dir') `Set.notMember` seen, pt `Set.notMember` walls, let path'' = Set.insert pt path']
                              <> [(cost + 1001, Map.singleton (pt, dir') path'') | let dir' = rotateLeft dir, let pt = (addCoord pos dir'), (pt, dir') `Set.notMember` seen, pt `Set.notMember` walls, let path'' = Set.insert pt path']
                              <> [(cost + 1, Map.singleton (pt, dir) path'') | let pt = (addCoord pos dir), (pt, dir) `Set.notMember` seen, pt `Set.notMember` walls, let path'' = Set.insert pt path']
                 ]
main :: IO ()
main = solve . parse . toCoord . lines <$> readFile "inputs/2024/input16.txt"
         >>= print
