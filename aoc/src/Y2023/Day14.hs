module Main ( main ) where 

import Utils ( toCoord )
import Rec
import Data.List ( transpose, sortBy, intercalate )
import Data.List.Split ( splitOn )
import qualified Data.Map as Map

parser = lines

sortRocks xs = rocks <> spots
  where rocks = filter (=='O') xs 
        spots = filter (=='.') xs 

sortLine = intercalate "#" . map sortRocks . splitOn "#"

west  = map sortLine
east  = map (reverse . sortLine . reverse)
north = transpose . west . transpose
south = transpose . east . transpose

load = sum . zipWith (*) [1..] . map (length . filter (=='O')) . reverse 

cycle' = east . south . west. north

findStable maps = maps !! (go Map.empty 0 maps)
  where 
    go seen i (x:xs)
      | x `Map.member` seen = let ix = seen Map.! x 
                               in (ix + (1000000000 - ix) `rem` (i - ix)) 
      | otherwise = go (Map.insert x i seen) (i+1) xs 

solve = load . findStable . iterate cycle'

main :: IO ()
main = solve . parser <$> readFile "inputs/2023/input14.txt"
         >>= print
