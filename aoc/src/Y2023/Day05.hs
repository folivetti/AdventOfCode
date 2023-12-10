{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
module Main ( main ) where 

import Utils ( runParser )
import Rec
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import Data.List ( foldl' )

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
            pure (seeds, humidity2loc . temp2humidity . light2temp . water2light . fert2water . soil2fert . seed2soil)

parseSingleRng = do dest <- decimal
                    space
                    src <- decimal
                    space
                    step <- decimal
                    pure (src, dest, step)

parseRng = do rngs <- parseSingleRng `sepBy` endOfLine
              let f x = maybe x id $ foldr g Nothing rngs
                          where
                            g (src, dest, step) Nothing = if x >= src && x < src+step
                                                             then Just (x - src + dest)
                                                             else Nothing
                            g _ (Just z) = Just z
              pure f

solve seeds seed2loc = (p1, p2)
    where
        p1 = minimum $ map seed2loc seeds
        p2 = minimum $ concatMap (\(x,y) -> map seed2loc [x .. x + y - 1]) $ pairOf seeds

        pairOf [] = []
        pairOf [_] = []
        pairOf (x:y:xs) = (x,y) : pairOf xs 

main :: IO ()
main = uncurry solve . (runParser parser) <$> B.readFile "inputs/2023/input05.txt"
         >>= print
