{-# language TypeApplications #-}
module Y2015.Day20 (solution) where

import Data.List ( findIndex, foldl' )
import Control.Monad ( guard )

input :: Int
input = 33100000
{-# inline input #-}

divisors :: (Int -> Bool) -> Int -> Int
divisors p n = foldl' add 0 [1 .. isqrt n]
  where
    add acc x = acc + sum (filter p $ f x)
    isqrt = floor @Double . sqrt . fromIntegral 
    f x = do let (q, r) = n `divMod` x
             guard $ r == 0
             if x==q then [x] else [x,q]
{-# inline divisors #-}

gifts, gifts2 :: Int -> Int
gifts    = (*10) . divisors (const True)
gifts2 n = (*11) . divisors (> (n-1) `div` 50) $ n

solution :: IO ()
solution = do
  print $ findIndex (>= input) . map gifts $ [0 ..]
  print $ findIndex (>= input) . map gifts2 $ [0 ..]
