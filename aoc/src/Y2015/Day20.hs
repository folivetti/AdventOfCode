module Y2015.Day20 (solution) where

import Data.List ( nub, findIndex, sort )
import Control.Monad ( guard )

input :: Int
input = 33100000

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral 

divisors :: Int -> [Int]
divisors n = nub $ concatMap f [1 .. isqrt n]
  where
    f x = do let (q, r) = n `divMod` x
             guard $ r == 0
             [x, q]

gifts, gifts2 :: Int -> Int
gifts = (*10) . sum . divisors

gifts2 n = (*11) . sum . filter (> (n-1) `div` 50) . divisors $ n

solution :: IO ()
solution = do
  print $ fmap (+1) . findIndex (>= input) . map gifts $ [1 ..]
  print $ fmap (+1) . findIndex (>= input) . map gifts2 $ [1 ..]
