module Main where

import Data.Sequence
import Prelude hiding (length, splitAt)


josephus :: Int -> Int
josephus n = 1 + 2 * (n - 2 ^ floor (logBase 2 $ fromIntegral n))

solve :: (Seq Int -> Int) -> Int -> Int
solve f = (`index` 0) . go . toSeq
  where
    go        = until onlyOne (rotate . getGift)
    onlyOne   = (==1) . length
    getGift x = deleteAt (f x) x
    rotate xs = let (x, y) = splitAt 1 xs in y <> x

    toSeq :: Int -> Seq Int
    toSeq = fromList . enumFromTo 1

main :: IO ()
main = do
  print $ josephus 3012210
  print $ solve ((`div` 2) . length) 3012210
