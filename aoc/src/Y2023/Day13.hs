{-# LANGUAGE MultiWayIf #-}
module Main ( main ) where 

import Utils ( runParser )
import Rec
import Data.List ( groupBy, transpose, inits, tails )
import Data.Monoid ( Sum(..) )

getDiffs xs = length . filter id . zipWith (/=) xs 
isRefl xs   = sum . zipWith getDiffs (reverse xs)
partitions xs = zip (inits xs) (tails xs)

findReflection :: Int -> [String] -> (Sum Int, Sum Int)
findReflection mul = cata alg . fromIList . partitions
  where 
    alg INilF = (0, 0)
    alg (IConsF ix (x,y) xs) = if | null x -> xs 
                                  | null y -> xs 
                                  | otherwise -> case isRefl x y of
                                                   0 -> (Sum (mul * ix), snd xs)
                                                   1 -> (fst xs, Sum (mul * ix))
                                                   _ -> xs
                                    
findBoth :: [String] -> (Sum Int, Sum Int)
findBoth xs = findReflection 1 (transpose xs) <> findReflection 100 xs 

solve xs = mconcat (map findBoth xs) -- (sum $ map (solver 0) xs, sum $ map (solver 1) xs)

main :: IO ()
main = solve . map (filter (/="")) . groupBy (\_ y -> y /= "") . lines <$> readFile "inputs/2023/input13.txt"
         >>= print
