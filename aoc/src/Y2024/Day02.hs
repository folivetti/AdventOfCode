{-# LANGUAGE TypeApplications #-}
module Main ( main ) where

import Utils

allCombs :: [Int] -> [[Int]]
allCombs xs = let n = length xs
              in [take i xs <> drop (i+1) xs | i <- [0..n]]

solve :: [[Int]] -> (Int, Int)
solve xss = (length $ filter (isSafe . calculateDiff) xss, length $ filter (any (isSafe . calculateDiff) . allCombs) xss)
  where
    calculateDiff xs = zipWith (\x y -> y-x) xs (tail xs)
    isSafe xs = all (\x -> abs x > 0 && abs x < 4) xs && (all (<0) xs || all (>0) xs)

main :: IO ()
main = solve . map (map (read @Int) . words) . lines <$> readFile "inputs/2024/input02.txt"
         >>= print
