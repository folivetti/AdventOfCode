{-# language TypeApplications #-}
module Main where

import Data.List ( subsequences )

main :: IO ()
main = do
  content <- map (read @Int) . lines <$> readFile "inputs/2015/input17.txt"
  let allcombs      = subsequences content
      minContainers = minimum $ map length part1
      part1         = filter ((==150).sum) allcombs
      part2         = filter ((==minContainers).length) part1
  print $ length part1
  print $ length part2
