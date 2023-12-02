module Main where

import Data.List ( nub )

next :: String -> String
next = reverse . add 1 . reverse
  where
    add _ []       = []
    add 0 xs       = xs
    add 1 ('z':xs) = 'a' : add 1 xs
    add n (x:xs)   = x' : add 0 xs
      where
        x' = toEnum $ fromEnum x + n

rule1, rule2, rule3 :: String -> Bool
rule1 xs = any p triples
  where
    p (a, b, c) = succ a == b && succ b == c
    triples     = zip3 xs (tail xs) (tail $ tail xs)
rule2 xs = 'i' `notElem` xs || 'o' `notElem` xs || 'l' `notElem` xs
rule3 xs = (>1) $ length $ nub $ filter (uncurry (==)) tuples
  where
    tuples = zip xs (tail xs)

isValid :: String -> Bool
isValid xs = rule1 xs && rule2 xs && rule3 xs

main :: IO ()
main = do
  let input = "vzbxkghb"
      passwords = filter isValid $ iterate next input
  print $ head passwords
  print $ passwords !! 1
