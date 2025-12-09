module Main where 

import Control.Arrow ( (&&&) )
import Data.Map ( Map ) 
import qualified Data.Map as Map
import Data.List ( sortBy )
import Data.Function ( on )
import qualified Data.DisjointSet as DS 

parse :: String -> [Int]
parse = map read . words . map (\c -> if c==',' then ' ' else c) 

makePoints :: [[Int]] -> [(Int, (Int, Int, Int))]
makePoints points = zip [0..] [(x,y,z) | [x,y,z] <- points]

dist :: (Int, (Int, Int, Int)) -> (Int, (Int, Int, Int)) -> Int
dist (_, (x1,y1,z1)) (_, (x2,y2,z2)) = (x1 - x2)^2 + (y1 - y2)^2 + (z1 - z2)^2 

solve1 points = product 
              . take 3 
              . sortBy (flip compare) 
              . map length . DS.toLists
              . foldr (\((i,_), (j,_)) acc -> DS.union i j acc) DS.empty 
              . take 1000 
              $ sortBy (compare `on` uncurry dist) [ (p1, p2) | p1 <- points, p2 <- points, fst p1 < fst p2 ]

solve2 points = snd . head $ dropWhile (\(s, _) -> DS.values s < n || DS.sets s > 1) $ scanl (\(s, _) ((i, (xi, _, _)), (j, (xj, _, _))) -> (DS.union i j s, xi * xj)) (DS.empty, 0)
              $ sortBy (compare `on` uncurry dist) [ (p1, p2) | p1 <- points, p2 <- points, fst p1 < fst p2 ]
                  where n = length points

main :: IO ()
main = (solve1 &&& solve2) . makePoints . map parse . lines <$> readFile "inputs/2025/input08.txt"
           >>= print
