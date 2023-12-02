module Main where

import Data.List ( nub )

isMarker :: Eq a => Int -> [a] -> Bool
isMarker n xs = length ys == length zs 
    where 
        ys = take n xs
        zs = nub ys

startOf :: Eq a => Int -> [a] -> Int
startOf n xs
  | isMarker n xs = n
  | otherwise     = 1 + startOf n (tail xs)

main :: IO ()
main = do content <- readFile "inputs/2022/input06.txt"
          print $ startOf 4 content
          print $ startOf 14 content
