module Main where

row, col :: Int
row = 2981
col = 3075

getIx :: (Int, Int) -> Int
getIx (r, c) = (r + c - 1) * (r + c - 2) `div` 2 + c - 1
{-# inline getIx #-}

step :: Int -> Int
step x = (x * 252533) `mod` 33554393
{-# inline step #-}

iterateUntil :: (Int -> Int) -> Int -> Int -> Int
iterateUntil f x0 n = go x0 0
  where
      go x m
        | m == n    = x
        | otherwise = go (f x) (m+1)

main :: IO ()
main = print $ iterateUntil step 20151125 $ getIx (row, col)
