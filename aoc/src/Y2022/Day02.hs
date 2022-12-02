{-# language ViewPatterns #-}
module Y2022.Day02 ( solution ) where

data Enemy = A | B | C deriving (Read, Show, Enum)
data Santa = X | Y | Z deriving (Read, Show, Enum)

point :: Enum a => a -> Int
point = (+1) . fromEnum

match, match2 :: (Enum a, Enum b) => a -> b -> Int
match (fromEnum -> x) (fromEnum -> y)
  | x == y = 3
  | (x+1) `mod` 3 == y = 6 -- 0 1, 1 2, 2 0
  | otherwise = 0

match2 (fromEnum -> x) (fromEnum -> y) = 
    case y of
      0 -> (x + 2) `mod` 3 + 1
      1 -> x + 1 + 3
      2 -> (x + 1) `mod` 3 + 7

score :: (Enum a, Enum b) => a -> b -> Int
score x y = match x y + point y

parse :: [String] -> (Enemy, Santa)
parse [x,y] = (read x, read y)
parse _ = error "no parser"

solution :: IO ()
solution = do content <- map (parse . words) . lines <$> readFile "inputs/2022/input02.txt"
              print $ sum $ map (uncurry score) content
              print $ sum $ map (uncurry match2) content
