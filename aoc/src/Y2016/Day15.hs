module Y2016.Day15 ( solution ) where

-- taken from https://stackoverflow.com/questions/35529211/chinese-remainder-theorem-haskell
crt :: (Integral a, Foldable t) => t (a, a) -> (a, a)
crt = foldr go (0, 1)
    where
    go (r1, m1) (r2, m2) = (r `mod` m, m)
        where
        r = r2 + m2 * (r1 - r2) * (m2 `inv` m1)
        m = m2 * m1

    -- Modular Inverse
    a `inv` m = let (_, i, _) = egcd a m in i `mod` m

    -- Extended Euclidean Algorithm
    egcd 0 b = (b, 0, 1)
    egcd a b = (g, t - (b `div` a) * s, s)
         where (g, s, t) = egcd (b `mod` a) a

myInput :: [(Int, Int)]
myInput = [ (-15, 17), (-2, 3), (-4, 19), (-2, 13), (-2, 7), (0, 5), (0, 11) ]

solve :: [(Int, Int)] -> Int
solve = fst . crt 

solution :: IO ()
solution = do let myData = zipWith (\ix (a, b) -> (a - ix, b)) [1..] myInput
              print $ solve $ init myData
              print $ solve myData
