
module Main where

import Control.Arrow 
import Data.List ( span, (\\) )
import Utils
import Data.Ranged ( (-?-) )
import qualified Data.Ranged as R
import Data.List.Split (splitOn)

parseRange :: String -> R.Range Integer
parseRange s = let [a,b] = splitOn "-" s in R.Range (R.BoundaryBelow $ read a) (R.BoundaryAbove $ read b)


getUpper (R.Range _ (R.BoundaryAbove b)) = b 
getLower (R.Range (R.BoundaryBelow a) _) = a 

solve1 xs = let (ranges, ids) = span (/="") xs
                rangeSet = R.makeRangedSet $ map parseRange ranges
             in length $ filter (rangeSet -?-) (map read $ tail ids)
                
solve2 xs = let (ranges, _) = span (/="") xs
                theranges = repeatUntilNoChange go $ map parseRange ranges
             in sum $ map (\r -> getUpper r - getLower r + 1) theranges
  where 
    repeatUntilNoChange f xs = let xs' = f xs 
                                in if xs' == xs then xs else repeatUntilNoChange f xs'
    go [] = []
    go [r] = [r]
    go (r:rs) = let rs' = filter (R.rangeOverlap r) rs 
                 in foldl (\acc x -> head $ R.rangeUnion acc x) r rs' : go (rs \\ rs')

main :: IO ()
main = (solve1 &&& solve2) . lines <$> readFile "inputs/2025/input05.txt"
         >>= print
