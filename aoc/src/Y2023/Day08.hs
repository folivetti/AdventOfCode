{-# language OverloadedStrings #-}
module Main ( main ) where 

import Utils ( runParser )
import Rec
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as Map
import Data.List ( foldl1 )

-- parser :: Parser ([(a,a) -> a], Map.Map String (String, String))
parser = do fs <- map toFun <$> many' (char 'L' <|> char 'R')
            many' space
            m <- many' parseKeyVal
            pure $ (fs, Map.fromList m)
  where 
    parseKeyVal = do k <- many' letter_ascii
                     string " = ("
                     v1 <- many' letter_ascii 
                     string ", "
                     v2 <- many' letter_ascii 
                     string ")"
                     many' space
                     pure (k, (v1,v2))

    toFun 'L' = fst 
    toFun 'R' = snd 

solve input = (part1 input, part2 input)

part1 (fs, m) = go (cycle fs) 0 "AAA"
  where
      go (g:gs) ix "ZZZ" = ix 
      go (g:gs) ix k     = go gs (ix+1) (g $ m Map.! k)

part2 (fs, m) = foldl1 lcm $ map (go (cycle fs) 0) keys 
  where
    keys      = filter ((`endWith` 'A')) $ Map.keys m
    endWith l c = (last l) == c

    go (g:gs) ix k
      | k `endWith` 'Z' = ix 
      | otherwise       = go gs (ix+1) (g $ m Map.! k)

main :: IO ()
main = solve . runParser parser <$> B.readFile "inputs/2023/input08.txt"
         >>= print
