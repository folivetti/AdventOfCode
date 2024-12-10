module Main where

import Data.Char
import Data.Sequence
import qualified Data.Sequence as Seq

checksum :: Int -> Seq (Either Int (Int, Int)) -> Int
checksum _ Empty = 0
checksum n (Left m    :<| xs) = checksum (n + m) xs
checksum n (Right (m, c) :<| xs) = sum (Prelude.take m $ map (* c) [n ..]) + checksum (n + m) xs

move1 Empty = Empty
move1 (Right (n ,c) :<| xs) = Right (n, c) :<| move1 xs
move1 (xs :|> Left n) = move1 xs :|> Left n
move1 (Left n :<| (xs :|> Right (m, c)))
  | n==m = Right (m, c) :<| move1 xs
  | n>m  = Right (m, c) :<| move1 (Left (n-m) :<| xs)
  | n<m  = Right (n, c) :<| move1 (xs :|> Right (m-n, c))

move1, move2 :: Seq (Either Int (Int, Int)) -> Seq (Either Int (Int, Int))
move2 Empty = Empty
move2 (xs :|> Left n) = move2 xs :|> Left n
move2 (xs :|> Right (file, c)) = case fit xs of
    Nothing -> move2 xs :|> Right (file, c)
    Just new -> move2 new :|> Left file
  where
    fit :: Seq (Either Int (Int, Int)) -> Maybe (Seq (Either Int (Int, Int)))
    fit Empty = Nothing
    fit (Left n :<| xs)
        | n >= file = Just (Right (file, c) :<| Left (n - file) :<| xs)
        | otherwise = (Left n :<|) <$> fit xs
    fit (x :<| xs) = (x :<|) <$> fit xs

solve1 :: Seq (Either Int (Int, Int)) -> Int
solve1 = checksum 0 . move1

solve2 :: Seq (Either Int (Int, Int)) -> Int
solve2 = checksum 0 . move2

solve xs = (solve1 xs, solve2 xs)

parseInput :: String -> Seq (Either Int (Int, Int))
parseInput = fromList . go 0
  where
    go _ []         = []
    go n [c]        = [Right (digitToInt c, n)]
    go n (c1:c2:cs) = Right (digitToInt c1, n) : Left (digitToInt c2) : go (n+1) cs

main :: IO ()
main = solve . parseInput <$> readFile "inputs/2024/input09.txt"
        >>= print
