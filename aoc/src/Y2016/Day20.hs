module Main where

import Data.Word ( Word32 )
import Data.List ( sortOn, foldl', find )

parse :: String -> (Word32, Word32)
parse xs = let (x1, x2) = span (/='-') xs
            in (read x1, read $ tail x2)

insertRange :: (Word32, Word32) -> [(Word32, Word32)] -> [(Word32, Word32)]
insertRange rng = merge . sortOn fst . (rng:)
  where
    merge []       = []
    merge [x]      = [x]
    merge ((a,b):(c,d):xs)
      | c <= b     = merge $ (a, max b d) : xs
      | otherwise  = (a,b) : merge ((c,d):xs)

part1 :: [(Word32, Word32)] -> Maybe Word32
part1 = fmap ((+1) . snd . fst) . find (\(x,y) -> fst y - snd x > 1) . withItself
  where withItself xs = zip xs $ tail xs

part2 :: [(Word32, Word32)] -> Word32
part2 []       = 0
part2 [x]      = 4294967295 - snd x
part2 (x:y:xs) = (fst y - snd x - 1) + part2 (y:xs)

main :: IO ()
main = do content <- map parse . lines <$> readFile "inputs/2016/input20.txt"
          let blacklist = foldl' (flip insertRange) [] content
          print $ part1 blacklist
          print $ part2 blacklist
