module Main ( main ) where

import Utils
import Data.List ( transpose, sort )

parse :: String -> [Int]
parse = map (\c -> read [c])

solve :: [[Int]] -> (Int, Int)
solve = foldr (\(a,b) (acc1, acc2) -> (a+acc1, b+acc2)) (0,0) . map (\j -> (findHighestJoltN 2 0 j, findHighestJoltN 12 0 j))
  where
    findHighestJolt :: [Int] -> Int
    findHighestJolt js = let md = maximum (init js)
                             js' = dropWhile (< md) js
                          in md * 10 + maximum (tail js')
    findHighestJoltN :: Int -> Int -> [Int] -> Int
    findHighestJoltN 0 r _  = r
    findHighestJoltN n r js = let l = length js 
                                  md = maximum (take (l-n+1) js)
                                  js' = dropWhile (< md) js
                               in findHighestJoltN (n-1) (r * 10 + md) (tail js')
main :: IO ()
main = solve . map parse . lines <$> readFile "inputs/2025/input03.txt"
         >>= print
