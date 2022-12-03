module Y2022.Day03 ( solution ) where

import Data.List ( find )
import Data.List.Split ( chunksOf )

findChar :: String -> Maybe Char
findChar xs = find (`elem` zs) $ take n xs 
  where
    n  = length xs `div` 2
    zs = drop n xs

findBadge :: [String] -> Maybe Char
findBadge [as, bs, cs] = find (\x -> x `elem` bs && x `elem` cs) as

priority :: Char -> Int
priority = adjust . fromEnum
  where
    adjust x
      | x >= 97 && x <= 122 = x - 96
      | x >= 65 && x <= 90  = x - 38
      | otherwise = x

sumMaybe f = foldr (addPriorities f) (Just 0)
addPriorities f x acc = (+) <$> acc <*> (priority <$> f x)

solution :: IO ()
solution = do content <- lines <$> readFile "inputs/2022/input03.txt"
              print $ sumMaybe findChar content
              print $ sumMaybe findBadge $ chunksOf 3 content
