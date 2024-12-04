{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import Utils
import Data.Array

toArr xss = listArray ((1, 1), (m, n)) $ concat xss
  where
    m = length xss
    n = length $ head xss

solve xss = ( sum [countXmas ix iy | ix <- [x0..xn], iy <- [y0..yn]]
            , sum [countX_mas ix iy | ix <- [x0..xn], iy <- [y0..yn]]
            )

  where
    ((x0, y0), (xn, yn)) = bounds xss
    countHorizontal ix iy
      | ix + 3 > xn = 0
      | otherwise   = let xmas = [xss ! (ix+i, iy) | i <- [0..3]]
                      in if xmas == "XMAS" || xmas == "SAMX"
                            then 1 else 0
    countVertical ix iy
      | iy + 3 > yn = 0
      | otherwise   = let xmas = [xss ! (ix, iy+i) | i <- [0..3]]
                      in if xmas == "XMAS" || xmas == "SAMX"
                            then 1 else 0
    countDiagonal ix iy
      | iy + 3 > yn || ix + 3 > xn = 0
      | otherwise   = let xmas = [xss ! (ix+i, iy+i) | i <- [0..3]]
                      in if xmas == "XMAS" || xmas == "SAMX"
                            then 1 else 0
    countOtherDiagonal ix iy
      | iy + 3 > yn || ix - 3 < x0 = 0
      | otherwise   = let xmas = [xss ! (ix-i, iy+i) | i <- [0..3]]
                      in if xmas == "XMAS" || xmas == "SAMX"
                            then 1 else 0
    countXmas ix iy = countHorizontal ix iy
                    + countVertical ix iy
                    + countDiagonal ix iy
                    + countOtherDiagonal ix iy
    countX_mas ix iy
      | xss ! (ix, iy) /= 'A' = 0
      | ix - 1 < x0 || ix + 1 > xn || iy - 1 < y0 || iy + 1 > yn = 0
      | otherwise = let mas1 = [xss ! (ix-1, iy-1), xss ! (ix, iy), xss ! (ix+1, iy+1)]
                        mas2 = [xss ! (ix+1, iy-1), xss ! (ix, iy), xss ! (ix-1, iy+1)]
                    in if (mas1 == "MAS" || mas1 == "SAM") && (mas2 == "MAS" || mas2 == "SAM")
                          then 1 else 0



main :: IO ()
main = solve . toArr . lines <$> readFile "inputs/2024/input04.txt"
        >>= print
