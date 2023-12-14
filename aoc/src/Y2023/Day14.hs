module Main ( main ) where 

import Utils ( toCoord )
import Rec
import Data.List ( transpose, sortBy, intercalate )
import Data.List.Split ( splitOn )
import qualified Data.Map as Map
import Control.Arrow ( (&&&) )
import Data.Monoid ( Sum(..) )

parser = lines

sortRocks = joinStrs . cata alg . fromList 
  where
    joinStrs (Sum m, Sum n) = replicate m 'O' <> replicate n '.'
    alg NilF                = (Sum 0, Sum 0)
    alg (ConsF 'O' xs)      = (Sum 1, Sum 0) <> xs
    alg (ConsF '.' xs)      = (Sum 0, Sum 1) <> xs

sortLine = intercalate "#" . map sortRocks . splitOn "#"

west  = map sortLine
east  = map (reverse . sortLine . reverse)
north = transpose . west . transpose
south = transpose . east . transpose

countOs = cata alg . fromList
  where 
    alg NilF = 0 
    alg (ConsF 'O' xs) = xs + 1 
    alg (ConsF  _  xs) = xs 

load = cata alg . fromIList . reverse 
  where 
    alg INilF            = 0
    alg (IConsF ix x xs) = xs + (ix + 1) * countOs x

cycle' = east . south . west. north
    
findStablePoint maps = hylo alg coalg (maps, 0, Map.empty, Map.empty) 
  where
    alg (Value ans)   = ans
    alg (Delayed ans) = ans

    coalg ([], _, _, _) = Value []
    coalg (xs, i, seen, cache)
      | xs `Map.member` seen = let ix    = seen Map.! xs
                                   adjIx = ix + (1000000000 - ix) `rem` (i - ix)
                                in Value (cache Map.! adjIx)
      | otherwise            = Delayed (cycle' xs, i+1, Map.insert xs i seen, Map.insert i xs cache)

solve = (load . north) &&& (load . findStablePoint)

main :: IO ()
main = solve . parser <$> readFile "inputs/2023/input14.txt"
         >>= print
