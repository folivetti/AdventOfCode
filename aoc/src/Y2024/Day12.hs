module Main where

import Utils
import Data.Map.Strict hiding (map,filter,foldr)
import qualified Data.Set as Set
import Data.List (nub)
import Debug.Trace

solve farm = (foldr (price perimeter1) 0 plants, foldr (price perimeter2) 0 plants)
  where
    plants = go (Set.fromList $ keys farm) []

    go coords patches
      | Set.null coords = patches
      | otherwise = let (coord, coords') = Set.deleteFindMin coords
                        patch = dfs (farm ! coord) coord Set.empty
                    in go (Set.difference coords patch) (patch:patches)

    dfs :: Char -> (Int, Int) -> Set.Set (Int, Int) -> Set.Set (Int, Int)
    dfs plant coord@(x, y) patch
      | coord `Set.member` patch = patch
      | otherwise = case farm !? coord of
                      Nothing -> patch
                      Just p  -> if p /= plant
                                    then patch
                                    else let p1 = dfs plant (x-1, y) (Set.insert coord patch)
                                             p2 = dfs plant (x+1, y) p1
                                             p3 = dfs plant (x, y-1) p2
                                             p4 = dfs plant (x, y+1) p3
                                         in p4

    isCorner plant (c1,c2,c3) = c1 `Set.notMember` plant && (c2 `Set.notMember` plant || c3 `Set.member` plant)

    neighbors (x, y) = [(x-1,y), (x+1, y), (x, y-1), (x, y+1)]
    corners (x, y) = [((x-1,y), (x,y-1), (x-1, y-1)), ((x,y-1),(x+1,y),(x+1,y-1)), ((x+1,y), (x,y+1),(x+1,y+1)), ((x,y+1),(x-1,y),(x-1,y+1))]

    perimeter1 coords = filter (`Set.notMember` coords) . concatMap neighbors
    perimeter2 coords = filter (isCorner coords) . concatMap corners

    price perimeterFun coords acc =
      let area = Set.size coords
          perimeter = length $ perimeterFun coords $  Set.toList coords
      in acc + area * perimeter

main :: IO ()
main = solve . fromList . toCoord . lines <$> readFile "inputs/2024/input12.txt"
        >>= print
