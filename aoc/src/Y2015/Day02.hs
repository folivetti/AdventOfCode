module Y2015.Day02 (solution) where

import Data.List.Split
import Data.List ( sort )

parseData :: String -> (Int, Int, Int)
parseData css = case splitOn "x" css of
                  [cx,cy,cz] -> (read cx, read cy, read cz)
                  _          -> error "parse error"

calcTot :: (Int, Int, Int) -> Int
calcTot (l, w, h) = 2*l*w + 2*w*h + 2*h*l + spare
  where spare = minimum [l*w, w*h, h*l]

calcRibbon :: (Int, Int, Int) -> Int
calcRibbon (l, w, h) = (l*w*h +) $ (2*) $ sum $ take 2 $ sort [l,w,h]

solution :: IO ()
solution = do
  content <- readFile "inputs/2015/input02.txt"
  let
    dims  = map parseData $ lines content
    part1 = sum $ map calcTot dims
    part2 = sum $ map calcRibbon dims
  print part1
  print part2
