{-# LANGUAGE TypeApplications #-}
module Main ( main ) where

import Utils
import Data.List ( transpose, sort )
import Data.List.Split ( splitOn )

parse :: String -> [(Int, Int)]
parse = map (getVals . splitOn "-") . splitOn ","
  where getVals [a, b] = (read a, read b)

checkRep :: Int -> Bool 
checkRep n = let n' = show n 
                 s  = length n' `div` 2
              in (take s n') == (drop s n')

checkRep2 :: Int -> Bool
checkRep2 n = let n' = show n 
                  xs = [seqOfn i n' | i <- [1..length n' `div` 2]]
               in any allEqual xs

seqOfn :: Int -> String -> [String] 
seqOfn n [] = []
seqOfn n str = take n str : seqOfn n (drop n str)

allEqual :: [String] -> Bool 
allEqual [] = True
allEqual (x:xs) = all (== x) xs

solve :: [(Int, Int)] -> (Int, Int)
solve = foldr addInvalids (0, 0)
  where
      addInvalids (a, b) (acc1, acc2) = (acc1 + sum [ n | n <- [a..b], checkRep n ], acc2 + sum [ n | n <- [a..b], checkRep2 n ])

main :: IO ()
main = solve . parse <$> readFile "inputs/2025/input02.txt"
         >>= print
