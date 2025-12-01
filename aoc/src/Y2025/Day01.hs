{-# LANGUAGE TypeApplications #-}
module Main ( main ) where

import Utils
import Data.List ( transpose, sort )

parse :: String -> (Char, Int)
parse (c:ns) = (c, read ns)

solve :: [(Char, Int)] -> (Int, Int)
solve = getAns . foldl count0 (50, 0, 0)
  where
      getAns (_, c1, c2) = (c1, c2)
      count0 (acc, c1, c2) ('L', n) = let n' = (acc - n) `mod` 100 
                                          z  = if acc == 0 
                                                  then n `div` 100
                                                  else (n - acc) `div` 100 + 1
                                       in (n', c1 + if n' == 0 then 1 else 0, c2 + abs z)
      count0 (acc, c1, c2) ('R', n) = let n' = (acc + n) `mod` 100 
                                          z  = (acc + n) `div` 100
                                       in (n', c1 + if n' == 0 then 1 else 0, c2 + z)

main :: IO ()
main = solve . map parse . lines <$> readFile "inputs/2025/input01.txt"
         >>= print
