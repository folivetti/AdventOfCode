{-# language DeriveFunctor #-}
module Main ( main ) where 

import Utils ( runParser )
import Rec
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import Data.List ( intercalate )
import Control.Arrow ( (&&&) )
import Data.Array ( listArray, (!), bounds )

parser = do spring <- many' (char '.' <|> char '#' <|> char '?')
            space
            nums <- decimal `sepBy` char ','
            pure (spring, nums)

unfoldSprings (xs, ys) = (intercalate "?" $ replicate 5 xs, concat $ replicate 5 ys)

takeArr ix n arr = [arr ! i | i <- [ix .. ix + n - 1], i <= snd (bounds arr)]
getArr ix arr
  | ix >= lo && ix <= hi = Just (arr ! ix) 
  | otherwise            = Nothing 
  where (lo, hi) = bounds arr

canBlock   = (`elem` ("?#" :: String))
noBlocks   = all (/='#')

-- dynamic programming
fillUp (springs, vals) = dyna alg coalg (0, 0)
  where
    rngSprings = [0 .. nSprings - 1]
    rngVals    = [0 .. nVals - 1]
    nSprings   = length springs 
    nVals      = length vals
    aSprings   = listArray (0, nSprings - 1) springs 
    aVals      = listArray (0, nVals - 1) vals 

    coalg (ix1, ix2)
      | ix1 >= nVals && ix2 >= nSprings = NilF 
      | ix2 >= nSprings                 = ConsF (ix1, nSprings) (ix1 + 1, 0)
      | otherwise                       = ConsF (ix1, ix2) (ix1, ix2 + 1)

    alg NilF = 0
    alg (ConsF (ix, iy) table)
      | iy >= nSprings = 0
      | ix >= nVals    = 0
      | otherwise      = case aSprings ! iy of 
                           '.' -> dot 
                           '#' -> hash 
                           '?' -> dot + hash
                           _   -> error "invalid char"
      where 
        offset m n = m * (nSprings + 1) + n - 1
        dot        = index table (offset 0 1)
        curNum     = aVals ! ix
        block      = takeArr iy curNum aSprings
        hash | length block == curNum && all canBlock block =
                     case getArr (iy + curNum) aSprings of
                       Nothing  -> if ix + 1 == nVals then 1 else 0
                       Just '#' -> 0
                       Just _   -> if ix + 1 == nVals 
                                    then if noBlocks (takeArr (iy+curNum+1) nSprings aSprings) then 1 else 0
                                    else index table (offset 1 (curNum+1))
             | otherwise = 0

solve1 = sum . map fillUp 
solve2 = sum . map (fillUp . unfoldSprings)

solve = (solve1 &&& solve2)

main :: IO ()
main = solve . map (runParser parser) . B.lines <$> B.readFile "inputs/2023/input12.txt"
         >>= print
