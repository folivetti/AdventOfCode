module Main ( main ) where 

import Rec
import Data.List.Split ( splitOn )
import Data.Char ( ord )
import Data.List ( find )
import qualified Data.Map.Strict as Map 
import Control.Arrow ( (&&&) ) 

hash = cata alg . fromList . reverse
  where 
    alg NilF = 0 
    alg (ConsF x xs) =  ((ord x + xs) * 17) `rem` 256

solve1 = cata alg . fromList
  where 
    alg NilF         = 0
    alg (ConsF x xs) = hash x + xs

getLabel = takeWhile (\x -> x /= '=' && x /= '-')

getValue :: String -> Int 
getValue = read . tail . dropWhile (/= '=')

insertOrReplace (label, value) xs = 
    case find ((==label) . fst) xs of 
      Nothing -> xs <> [(label, value)]
      Just _  -> map (\(lbl, val) -> if lbl==label then (lbl, value) else (lbl, val)) xs

removeLabel lenId label lens = Map.filter (not.null) . Map.insert lenId (filter ((/=label).fst) lens)

calcSlots = sum . zipWith (\ix (_,v) -> ix*v) [1..] 
calcLens  = sum . map (\(ix, xs) -> (ix+1) * calcSlots xs) 

solve2 = calcLens . Map.toAscList . cata alg . fromList
  where 
    alg NilF = Map.empty 
    alg (ConsF x xs)
      | '=' `elem` x = Map.insert lenId (insertOrReplace (label, value) lens) xs
      | '-' `elem` x = if null lens 
                          then xs 
                          else removeLabel lenId label lens xs
      where 
        label = getLabel x
        value = getValue x
        lenId = hash label
        lens  = Map.findWithDefault [] lenId xs

                           

main :: IO ()
main = (solve1 &&& solve2) . reverse . splitOn "," . filter (/='\n') <$> readFile "inputs/2023/input15.txt"
         >>= print
