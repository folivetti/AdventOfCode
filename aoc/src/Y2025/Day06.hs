
module Main where

import Control.Arrow 
import Utils
import Data.List ( transpose )
import Debug.Trace ( traceShow )

parse :: [String] -> ([[Int]], [String])
parse ls = (transpose $ map (map read . words) initLines, words $ last ls)
  where
    initLines = take (length ls - 1) ls

solve1 input = let (nums, ops) = parse input in sum $ zipWith applyOp ops nums
  where
    applyOp "+" = sum
    applyOp "*" = product
    applyOp _   = const 0

solve2 input = let nums = map (map read) $ breakAll (all (==' ')) $ transpose (init input)
                   ops  = words $ last input 
                in sum $ zipWith applyOp ops nums
  where 
    applyOp "+" = sum
    applyOp "*" = product
    applyOp _   = const 0
    breakAll p [] = []
    breakAll p xs = let (ys, zs) = break p xs
                     in if null zs then [ys] else ys : breakAll p (tail zs)

main :: IO ()
main = (solve1 &&& solve2) . lines <$> readFile "inputs/2025/input06.txt"
         >>= print
