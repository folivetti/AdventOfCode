module Main where

import Data.Char
import Data.Map.Strict hiding (map)
import qualified Data.Map.Strict as Map
import Utils
import Data.Maybe
import Data.List ( nub )
import Control.Monad

getZeros = map fst . toList . Map.filter (==0)
solve1 xs = sum . map score . getZeros $ xs
  where
    score (x, y) = length . nub $ go 0 (x, y)
    go 9 (x, y) = [(x,y)]
    go n (x, y) = concatMap (go (n+1)) $
                    [c | c <- [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
                     , c `member` xs
                     , xs ! c == (n+1)]

solve2 xs = sum . map rating . getZeros $ xs
  where
    rating (x, y) = length . nub $ go 0 (x, y)
    go 9 (x, y) = [[(x,y)]]
    go n (x, y) = do c <- [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
                     guard $ c `member` xs && xs ! c == (n+1)
                     ((x,y):) <$> go (n+1) c

solve xs = (solve1 xs, solve2 xs)

parseInput = fromList . toCoord . map (map digitToInt)

main :: IO ()
main = solve . parseInput . lines <$> readFile "inputs/2024/input10.txt"
        >>= print
