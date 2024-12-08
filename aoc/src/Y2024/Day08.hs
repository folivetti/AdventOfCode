{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import Utils
import Data.Map.Strict hiding ( map, filter )
import qualified Data.Map.Strict as Map
import Data.List ( nub )

inMap (maxX, maxY) (x, y) = x >= 0 && x <= maxX && y >= 0 && y <= maxY

createAnti maxCoord coords = [an | (x1, y1) <- coords
                                 , (x2, y2) <- coords
                                 , (x1, y1) /= (x2, y2)
                                 , let an = (2*x1 - x2, 2*y1 - y2)
                                 , inMap maxCoord an
                             ]
createAntiT maxCoord coords = concat $
                               [an | (x1, y1) <- coords
                                   , (x2, y2) <- coords
                                   , (x1, y1) /= (x2, y2)
                                   , let an = takeWhile (inMap maxCoord) $ zip [x1, 2*x1 - x2 ..] [y1, 2*y1 - y2 ..]
                               ]

solve xss = (getResult part1, getResult part2)
  where
    getResult = length . nub . concatMap snd . toList
    maxCoord  = fst . last $ xss
    antennas  = map (\(a,b) -> (b, [a]))$ filter ((/='.') . snd) xss
    part1     = Map.map (createAnti maxCoord)  $ fromListWith (++) antennas
    part2     = Map.map (createAntiT maxCoord) $ fromListWith (++) antennas

main :: IO ()
main = solve . toCoord . lines <$> readFile "inputs/2024/input08.txt"
        >>= print
