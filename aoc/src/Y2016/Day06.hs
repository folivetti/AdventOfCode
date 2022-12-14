module Main where

import Data.List ( transpose, maximumBy, minimumBy, sort, group )
import Data.Function ( on )

decodeWith :: ((String -> String -> Ordering) -> [String] -> String) -> String -> Char
decodeWith f = head . f (compare `on` length) . group . sort

main :: IO ()
main = do content <- transpose . lines <$> readFile "inputs/2016/input06.txt"
          let part1 = map (decodeWith maximumBy) content
              part2 = map (decodeWith minimumBy) content
          putStrLn part1
          putStrLn part2
