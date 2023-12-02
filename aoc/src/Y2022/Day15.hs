{-# language OverloadedStrings #-}
module Main where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import Data.List ( sort, foldl', nub )
import Data.Maybe ( mapMaybe )
import Utils ( runParser )

type Coord = (Int, Int)
type Coords = (Coord, Coord)

parser :: Parser Coords
parser = do string "Sensor at x="
            x1 <- signed decimal
            string ", y="
            y1 <- signed decimal
            string ": closest beacon is at x="
            x2 <- signed decimal
            string ", y="
            y2 <- signed decimal
            pure ((x1, y1), (x2, y2))

manhattan :: Coord -> Coord -> Int
manhattan (a, b) (c, d) = abs (a - c) + abs (b - d)

findNoBeacon row sensors = reverse $ merge range
  where range = sort [ (a - x, a + x) | ((a,b), (c,d)) <- sensors 
                                      , let x = manhattan (a,b) (c,d) - abs (row - b)
                                      , x >= 0
                     ]
        merge = foldl' f []
        f [] x = [x]
        f (y:ys) x | fst x <= snd y = (fst y, max (snd y) (snd x)) : ys
                   | otherwise     = x:y:ys

part1 sensors = subtract beacons $ sum $ map width $ findNoBeacon row sensors
  where beacons = length $ nub $ [a | (_, (a, b)) <- sensors, b == row]
        width (a, b) = b - a + 1
        row = 2000000

part2 sensors = freq $ head $ mapMaybe findBeacon [0 .. maxVal]
  where
    maxVal = 4000000
    freq (x, y) = x*maxVal + y
    -- take into consideration there is only a single possible spot
    -- thus, every call to findNoBeacon shoul return a list with a single element
    -- the x coordinate available will either be 0 or 4000000
    findBeacon y = case findNoBeacon y sensors of
                     [] -> Just (0, y) -- all spaces are possible, take first
                     ((a, b):_)  | a > 0 -> Just (0, y) -- (0, y) is available
                                 | b < maxVal -> Just (b+1, y)
                                 | otherwise -> Nothing -- no space available


main :: IO ()
main = do content <- B.lines <$> B.readFile "inputs/2022/input15.txt"
          let sensors = map (runParser parser) content
          print $ part1 sensors
          print $ part2 sensors
