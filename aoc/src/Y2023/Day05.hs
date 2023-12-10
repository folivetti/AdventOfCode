{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
module Main ( main ) where 

import Utils ( runParser )
import Rec
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import Data.List ( sortOn )
import Control.Monad ( guard )
import Data.Maybe ( listToMaybe, mapMaybe )

-- parser :: Parser (Int, Set.Set Int, Set.Set Int)
parser = do string "seeds: "
            seeds <- decimal `sepBy` space
            many' space >> string "seed-to-soil map:" >> many' space
            seed2soil <- parseRng
            many' space >> string "soil-to-fertilizer map:" >> many' space
            soil2fert <-  parseRng
            many' space >> string "fertilizer-to-water map:" >> many' space
            fert2water <-  parseRng
            many' space >> string "water-to-light map:" >> many' space
            water2light <- parseRng
            many' space >> string "light-to-temperature map:" >> many' space
            light2temp <- parseRng
            many' space >> string "temperature-to-humidity map:" >> many' space
            temp2humidity <- parseRng
            many' space >> string "humidity-to-location map:" >> many' space
            humidity2loc <- parseRng
            let rngs = map (sortOn getSrc) [seed2soil, soil2fert, fert2water, water2light, light2temp, temp2humidity, humidity2loc]
            pure (seeds, rngs)

parseSingleRng = do dest <- decimal
                    space
                    src <- decimal
                    space
                    step <- decimal
                    pure (src, dest, step)

parseRng = parseSingleRng `sepBy` endOfLine

nonEmpty (_, _, l) = l > 0
getSrc (x, _, _)   = x
getDest (_, x, _)  = x
getLen (_, _, x)   = x

-- * taken from https://github.com/gruhn/advent-of-code/blob/master/2023/Day05.hs
--
fillRanges :: [(Int, Int, Int)] -> [(Int, Int, Int)]
fillRanges rngs = accu st alg (fromList rngs) 0 
  where 
    st NilF _         = NilF
    st (ConsF x xs) _ = ConsF x (xs, getSrc x + getLen x)

    alg NilF s         = [(s, s, maxBound - s)]
    alg (ConsF x xs) s = (s, s, getSrc x - s) : x : xs

mergeRngs :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int, Int)]
mergeRngs [] rs = rs 
mergeRngs rs [] = rs
mergeRngs rngs1 rngs2 = do 
    (srcA, dstA, lenA) <- rngs1
    (srcB, dstB, lenB) <- rngs2 
    let dest = dstB + max 0 (dstA - srcB)
        src  = srcA + max 0 (srcB - dstA)
        len  = min (dstA + lenA) (srcB + lenB) - max dstA srcB 
    guard $ nonEmpty (src, dest, len)
    pure (src, dest, len)

lookupSeed seed = cata alg . fromList
    where 
        alg NilF = seed 
        alg (ConsF (src, dest, len) xs) = 
            if src <= seed && seed <= (src + len)
              then seed + dest - src 
              else xs 

rangeMapFromSeeds = toList . ana coalg
  where 
    coalg []       = NilF
    coalg [_]      = NilF
    coalg (x:y:xs) = ConsF (x, x, y) xs

solve :: [Int] -> [[(Int, Int, Int)]] -> (Int, Int)
solve seeds rs = (cata alg1 (fromList seeds)
                 , cata alg2 allRngs)
    where
        seed2loc = cata loc (fromList rs)
        allRngs  = fromList $ mergeRngs (rangeMapFromSeeds seeds) seed2loc

        loc NilF         = []
        loc (ConsF x xs) = mergeRngs (fillRanges x) xs

        alg1 NilF         = maxBound
        alg1 (ConsF x xs) = min xs (lookupSeed x seed2loc)

        alg2 NilF         = maxBound
        alg2 (ConsF x xs) = min xs (getDest x)
        
main :: IO ()
main = uncurry solve . (runParser parser) <$> B.readFile "inputs/2023/input05.txt"
         >>= print
