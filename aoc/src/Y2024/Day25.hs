module Main where

import Data.Either 

parse :: [String] -> [Either [Int] [Int]]
parse = go 
  where
    tail' [] = []
    tail' xs = tail xs
    go [] = []
    go xss = let (keyOrLock, (rest)) = splitAt 7 xss
              in case head keyOrLock of
                   "#####" -> Left (parseThing keyOrLock) : go (tail' rest)
                   _       -> Right (parseThing keyOrLock) : go (tail' rest)
    parseThing = foldr (\x acc -> zipWith (+) acc $ map (fromEnum . (=='#')) x) (replicate 5 (-1))

f &&& g = \x -> (f x, g x)

solve1 xss = foldr (\x acc -> uncurry match x + acc) 0 
           $ (,) <$> keys <*> locks
  where 
      keys  = filter isRight xss 
      locks = filter isLeft xss 
      match (Right x) (Left y) 
        | all (<=5) $ zipWith (+) x y = 1
        | otherwise = 0

solve2 = const True 

solve = solve1 &&& solve2

main :: IO ()
main = solve . parse . lines <$> readFile "inputs/2024/input25.txt"
         >>= print
