{-# language OverloadedStrings #-}
module Main ( main ) where 

import Utils ( runParser )
import Rec
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B

parser :: Parser [(Int, Int)]
parser = do string "Time:" >> many' space
            times <- decimal `sepBy` (many' space)
            many' space >> string "Distance:" >> many' space 
            dists <- decimal `sepBy` many' space
            pure $ zip times dists

getRange t d = (x0, x1)
  where 
    t'    = fromIntegral t 
    d'    = fromIntegral d
    delta = sqrt $ t'^2 - 4*d'
    x0    = (t' - delta) / 2 
    x1    = (t' + delta) / 2

roundRange (a, b) = (a', b')
  where
    isIntegral x = fromIntegral (round x) == x 

    a' = if isIntegral a then round a + 1 else ceiling a
    b' = if isIntegral b then round b - 1 else floor b

numOfWins (a, b) = b - a + 1

solve :: [(Int, Int)] -> (Int, Int)
solve = getVals . cata alg . fromList 
  where
      alg NilF                    = (1, ("", ""))
      alg (ConsF (t, d) (p1, p2)) = (getWins (t, d) * p1, (show t, show d) <> p2)

      getWins        = numOfWins . roundRange . uncurry getRange
      getVals (a, b) = (a, getWins (read (fst b), read (snd b)))

main :: IO ()
main = solve . runParser parser <$> B.readFile "inputs/2023/input06.txt"
         >>= print
