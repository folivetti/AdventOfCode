module Main where

import Data.List
import Data.CircularList
import Data.Maybe

ex :: [Int]
ex = [1, 2, -3, 3, -2, 0, 4]

updateIx :: CList (Int, Int) -> Int -> Maybe (CList (Int, Int))
updateIx xs ix = do
    ys       <- findRotateTo ((==ix).fst) xs
    (iy, y)  <- focus ys
    pure $ insertL (iy, y) $ rotN (y `mod` n) $ removeL ys
        where n = length xs - 1

part1, part2 :: [Int] -> Maybe Int
part1 xs = do
  let xs' = mixAll $ fromList $ enumerate xs
  ys  <- toInfList <$> findRotateTo (==0) (fmap snd xs')
  pure $ ys !! 1000 + ys !! 2000 + ys !! 3000

part2 xs = do
  let xs' = (!! 10) $ iterate mixAll $ fromList $ enumerate xs
  ys  <- toInfList <$> findRotateTo (==0) (fmap snd xs')
  pure $ ys !! 1000 + ys !! 2000 + ys !! 3000

mixAll :: CList (Int, Int) -> CList (Int, Int)
mixAll xs = foldl' (\acc x -> fromJust $ updateIx acc x) xs [0 .. length xs - 1]

enumerate :: [b] -> [(Int, b)]
enumerate = zip [0..]

key :: Int
key = 811589153

main :: IO ()
main = do content <- map read . lines <$> readFile "inputs/2022/input20.txt"
          print $ part1 content
          print $ part2 $ map (*key) content
