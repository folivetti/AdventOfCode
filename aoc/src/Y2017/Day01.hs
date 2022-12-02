module Y2017.Day01 ( solution ) where

import Data.Char ( digitToInt )

part1 :: String -> [Int]
part1 xs = if head xs == last xs
             then digitToInt (head xs) : seqs
             else seqs
  where seqs = map (digitToInt . fst) $ filter (uncurry (==)) $ zip xs (tail xs)

solution :: IO ()
solution = do content <- init <$> readFile "inputs/2017/input01.txt"
              print $ sum $ part1 content
