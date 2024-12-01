{-# LANGUAGE TypeApplications #-}
module Main ( main ) where

import Utils
import Data.List ( transpose, sort )

solve :: [[Int]] -> (Int, Int)
solve xss = (sum $ zipWith (\x y -> abs (x-y)) xs1 xs2 , sum freq)
  where
    xss' = transpose xss 
    xs1  = sort $ xss' !! 0
    xs2  = sort $ xss' !! 1 
    freq = map calcFreq xs1 
    calcFreq x = x * foldr (\y acc -> if y==x then acc+1 else acc) 0 xs2

main :: IO ()
main = solve . map (map (read @Int) . words) . lines <$> readFile "inputs/2024/input01.txt"
         >>= print
