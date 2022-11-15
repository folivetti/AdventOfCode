module Y2016.Day03 ( solution ) where

import Data.List ( transpose )

type Triple = (Int, Int, Int)

isTriangle :: Triple -> Bool
isTriangle (x, y, z) = x + y > z && x + z > y && y + z > x

toTriple :: [Int] -> Triple
toTriple [a,b,c] = (a, b, c)
toTriple _       = error "wrong format"

regroup :: [Int] -> [[Int]]
regroup [] = []
regroup xs = take 3 xs : regroup (drop 3 xs)

howMany :: [[Int]] -> Int
howMany = length . filter isTriangle . map toTriple

getVerticals :: [[Int]] -> [[Int]]
getVerticals = regroup . concat . transpose

solution :: IO ()
solution = do
    content <- map (map read . words) . lines <$> readFile "inputs/2016/input03.txt"
    print $ howMany content
    print $ howMany . getVerticals $ content
