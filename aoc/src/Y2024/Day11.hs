module Main where

import Utils
import Data.MemoTrie

blink = rules
  where
    rules 0 = [1]
    rules x = case splitDigits x of
                Nothing -> [x*2024]
                Just xs -> xs

splitDigits :: Int -> Maybe [Int]
splitDigits x = if even n
                   then Just splt
                   else Nothing
  where
    toL (a, b) = [a,b]
    digits  = go x []
    n       = length digits
    splt    = map merge . toL $ splitAt (n `div` 2) digits
    merge   = foldl (\acc x -> acc*10 + x) 0
    go 0 xs = xs
    go n xs = go (n `div` 10) $ (n `mod` 10) : xs

count :: Int -> Int -> Int
count = go
  where
    go :: Int -> Int -> Int
    go 0 = const 1
    go n = memo $ sum . map (count (n-1)) . blink

run n = sum . map (count n)

solve xs = (run 25 xs, run 75 xs)

parseInput :: String -> [Int]
parseInput = map read . words

main :: IO ()
main = solve . parseInput <$> readFile "inputs/2024/input11.txt"
        >>= print
