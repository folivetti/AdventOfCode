{-# LANGUAGE TupleSections #-}
module Main where

import Data.Bits
import qualified Data.Map as M 

next :: Int -> Int
next x = step x
  where
    step = stepWith (*2048) . stepWith (`div` 32) . stepWith (*64)
    stepWith f x = (`mod` 16777216) $ xor x $ f x 

group xs@(x:y:z:w:_) = M.singleton (snd x, snd y, snd z, snd w) (fst w) : group (tail xs) 

diff xs = zip xs $ 0 : zipWith (-) (tail xs) xs

solve1 = sum . map ((!! 2000) . iterate next . read)
solve2 = M.foldr max 0 . M.unionsWith (+) . map (M.unions . take 1998 . group . diff . map (`mod` 10) . iterate next . read)

solve = solve1 &&& solve2 

f &&& g = \x -> (f x, g x) 

main :: IO ()
main = solve . words <$> readFile "inputs/2024/input22.txt"
         >>= print
         
