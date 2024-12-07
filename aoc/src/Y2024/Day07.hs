{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import Utils

f &&& g = \xs -> (f xs, g xs)

parse :: String -> (Integer, [Integer])
parse xss = let (eq : tks) = words xss
            in (read $ init eq, map read tks)

solve1 = sum . map fst . filter (uncurry (hasSol (-1)))
  where
    hasSol tot eq []     = tot == eq
    hasSol (-1) eq (x:xs) = hasSol x eq xs || hasSol x eq xs
    hasSol tot eq (x:xs) = hasSol (tot+x) eq xs || hasSol (tot*x) eq xs

solve2 = sum . map fst . filter (uncurry (hasSol (-1)))
  where
    hasSol tot eq []     = tot == eq
    hasSol (-1) eq (x:xs) = hasSol x eq xs || hasSol x eq xs
    hasSol tot eq (x:xs) = hasSol (tot+x) eq xs || hasSol (tot*x) eq xs || hasSol (concatNum tot x) eq xs

    concatNum a b = read (show a <> show b)

main :: IO ()
main = (solve1 &&& solve2) . map parse . lines <$> readFile "inputs/2024/input07.txt"
        >>= print
