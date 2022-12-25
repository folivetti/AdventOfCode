module Main where

import Data.List ( foldl' )

fromSNAFU :: String -> Int
fromSNAFU = foldl' (\acc c -> 5 * acc + f c) 0
  where
    f '0' = 0
    f '1' = 1
    f '2' = 2
    f '-' = -1
    f '=' = -2

toSNAFU :: Int -> String
toSNAFU = go ""
  where
    go snafu 0 = snafu
    go snafu n = go ("012=-" !! r : snafu) n'
      where
        (q, r) = n `divMod` 5
        n'     = if r <= 2 then q else q + 1

main :: IO ()
main = do content <- lines <$> readFile "inputs/2022/input25.txt"
          let tot = sum $ map fromSNAFU content
          print $ toSNAFU tot
