module Main ( main ) where 

import Utils ( toCoord )
import Rec
import Data.List ( groupBy, nub )
import Data.Char ( isDigit )
import qualified Data.Map as Map 

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x+a, y+b) | a <- [-1 .. 1], b <- [-1 .. 1], (a,b) /= (0,0)]

isPartNumber :: Map.Map (Int, Int) a -> ([(Int, Int)], Int) -> Bool 
isPartNumber m = any (`Map.member` m) . concatMap neighbors . fst

isContiguous :: ((Int, Int), Char) -> ((Int, Int), Char) -> Bool
isContiguous ((x,_), a) ((w,_), b) = x==w && isDigit a && isDigit b

solve :: [((Int, Int), Char)] -> (Int, Int)
solve xss = (part1, part2) 
  where
    part1 = cata alg1 (fromList numbers)
    part2 = cata alg2 (fromList symbols)

    alg1 NilF = 0
    alg1 (ConsF (cs,x) xs)
      | isPartNumber mapSymbs (cs, x) = x + xs
      | otherwise = xs

    alg2 NilF = 0
    alg2 (ConsF (c,"*") xs) = getGear c + xs
    alg2 (ConsF _ xs) = xs

    -- get a gear 
    getGear coord
      | length nums == 2 = product nums 
      | otherwise        = 0
      where nums = nub [mapNumbers Map.! c | c <- neighbors coord, c `Map.member` mapNumbers]

    -- change the lists into maps
    (numbers, symbols) = cata getMaps grouped 
    mapSymbs           = Map.fromList symbols
    mapNumbers         = Map.fromList [(c,x) | (cs,x) <- numbers, c <- cs]

    -- group data if there is a sequece of digits
    grouped       = fromList $ map unzip $ groupBy isContiguous xss
    allDigit      = all isDigit
    isSymb y      = all (not.isDigit) y && y /= "."
 
    -- get list of numbers and symbols
    getMaps NilF    = ([], [])
    getMaps (ConsF (cs,xs) (nums, symbs)) 
      | allDigit xs = ((cs, read xs):nums, symbs)
      | isSymb   xs = (nums, (head cs, xs):symbs)
      | otherwise   = (nums, symbs)
    

main :: IO ()
main = solve . toCoord . lines <$> readFile "inputs/2023/input03.txt"
         >>= print
