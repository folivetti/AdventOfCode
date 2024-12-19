module Main where

import Utils
import Data.List
import Data.MemoTrie

parse = (words . filter (/=',') . head) &&& (tail.tail)

f &&& g = \x -> (f x, g x)
solve = solve1 &&& solve2

solve1 (pats, designs) = length $ filter isPossible designs
  where
    isPossible ""     = True
    isPossible design = any (isPossible . removePat) $ filter (`isPrefixOf` design) pats
      where removePat pat = let n = length pat in drop n design

solve2 (pats, designs) = sum $ map build designs
  where
    build :: String -> Int
    build = memo $ \design -> if null design
                                 then 1
                                 else let pats' = (filter (`isPrefixOf` design) pats)
                                      in  sum $ map (\p -> let n = length p in build (drop n design)) pats'

main :: IO ()
main = solve . parse . lines <$> readFile "inputs/2024/input19.txt"
         >>= print
