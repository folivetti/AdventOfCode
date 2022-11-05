module Y2015.Day25 (solution) where

row, col :: Int
row = 2981
col = 3075

getIx :: (Int, Int) -> Int
getIx (r, c) = (r + c - 1) * (r + c - 2) `div` 2 + c - 1

step :: Int -> Int
step x = (x * 252533) `mod` 33554393

solution :: IO ()
solution = print $ iterate step 20151125 !! getIx (row, col)
