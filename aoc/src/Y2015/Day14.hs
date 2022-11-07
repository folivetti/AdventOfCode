{-# language OverloadedStrings #-}
module Y2015.Day14 (solution) where

import Utils ( runParser )
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8 ( decimal, isDigit, skipWhile, Parser )
import Data.List ( transpose )

parseReindeer :: Parser (Int, Int, Int)
parseReindeer = do
  skipWhile (not . isDigit)
  speed <- decimal
  skipWhile (not . isDigit)
  duration <- decimal
  skipWhile (not . isDigit)
  rest <- decimal
  pure (speed, duration, rest)

runTrace :: Int -> (Int, Int, Int) -> [Int]
runTrace time (speed, duration, rest) = init $ go time [0]
  where
    go t kms
        | duration >= t        = updateKms kms t
        | duration + rest >= t = delay (t - duration) $ updateKms kms duration
        | otherwise            = go (t - duration - rest) $ delay rest $ updateKms kms duration
    updateKms kms tf = [head kms + speed * x | x <- [tf, tf - 1 .. 1]] <> kms
    delay t kms = replicate t (head kms) <> kms

scores :: [[Int]] -> [Int]
scores = map sum . transpose . map f . transpose
  where
    f xs = let x = maximum xs
            in map (\y -> if x==y then 1 else 0) xs

solution :: IO ()
solution = do
    content <- B.lines <$> B.readFile "inputs/2015/input14.txt"
    let reindeers = map (runParser parseReindeer) content
        traces    = map (runTrace 2503) reindeers
        scs       = scores traces
    print $ maximum $ map head traces
    print $ map length traces
    print $ maximum scs
