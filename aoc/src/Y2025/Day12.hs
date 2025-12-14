module Main where 

import Utils
import Data.List ( break )


solve :: [String] -> Integer
solve = foldr count 0
  where
      count line acc
        | 'x' `notElem` line = acc 
        | otherwise = let (dim, boxes) = break (==':') line 
                          (w, h) = break (=='x') dim 
                          area = (read w * read (tail h)) `div` 9
                          total = foldr (\box acc -> acc + read box) 0 (words . tail . tail $ boxes)
                       in acc + if total <= area then 1 else 0
main :: IO ()
main = solve . lines <$> readFile "inputs/2025/input12.txt"
         >>= print
