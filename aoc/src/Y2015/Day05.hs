module Main where

import Data.List ( foldl' )

part1, part2 :: String -> Int
part1 str = fromEnum $ go str (0, False)
  where
      naughty = ["ab", "cd", "pq", "xy"]
      isVowel = fromEnum . (`elem` "aeiou")
      go [] (b1, b2) = b1 >= 3 && b2
      go [x] (b1, b2) = (b1 + isVowel x >= 3) && b2
      go (x:y:xs) (b1, b2)
        | [x,y] `elem` naughty = False
        | otherwise = go (y:xs) (b1 + isVowel x, b2 || x==y)
      
part2 str = fromEnum $ go str (False, False)
  where
      twice xy xs = xy `elem` zip xs (tail xs)
      go (x:y:z:xs) (b1, b2) = go (y:z:xs) (b1 || x==z, b2 || twice (x,y) (z:xs))
      go (x:y:xs) (b1, b2)   = go (y:xs) (b1, b2 || twice (x,y) xs)
      go _ (b1, b2)          = b1 && b2

main :: IO ()
main = do
  content <- lines <$> readFile "inputs/2015/input05.txt"
  let isNice = foldl' (\(a, b) x -> (a + part1 x, b + part2 x)) (0, 0)
  print $ isNice content
