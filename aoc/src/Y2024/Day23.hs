{-# LANGUAGE TupleSections #-}
module Main where

import qualified Data.Map as M 
import Data.List ( sort, nub, foldr1, intercalate )
import Data.Algorithm.MaximalCliques

createGraph :: [String] -> M.Map String [String]
createGraph = foldr (\(a, b) m -> M.insertWith (++) (tail b) [a] $ M.insertWith (++) a [tail b] m) M.empty . map (span (/='-'))

get3clique :: M.Map String [String] -> [[String]]
get3clique m = filter (any ((=='t').head)) 
             $ nub 
             $ [sort [a, b, c] | a <- M.keys m, b <- m M.! a, c <- m M.! b, a `elem` m M.! c]

solve1 = length . get3clique
solve2 m = intercalate "," $ sort $ maximumOn length $ getMaximalCliques isConnected (M.keys m)
  where 
      isConnected a b = b `elem` m M.! a 
      maximumOn f = foldr1 (\x y -> if f x > f y then x else y)

solve = solve1 &&& solve2 

f &&& g = \x -> (f x, g x) 

main :: IO ()
main = solve . createGraph . words <$> readFile "inputs/2024/input23.txt"
         >>= print
         
