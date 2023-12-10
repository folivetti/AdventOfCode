{-# language OverloadedStrings #-}
module Main ( main ) where 

import Utils ( runParser )
import Rec
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as Map
import Data.List ( foldl1 )
import Control.Arrow ( (&&&) )

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

getIx k p (fs, m) = hylo alg coalg (k, cycle fs)
  where
    coalg (k, (g:gs)) 
      | p k       = NilF 
      | otherwise = ConsF k (g (m Map.! k), gs)

    alg NilF = 0
    alg (ConsF x xs) = xs + 1

part1 = getIx "AAA" (=="ZZZ")

part2 (fs, m) = cata alg (fromList $ Map.keys m)
  where 
    endWith l c = (last l) == c

    alg NilF         = 1
    alg (ConsF x xs) 
      | x `endWith` 'A' = lcm xs $ getIx x (`endWith` 'Z') (fs, m)
      | otherwise       = xs

solve = part1 &&& part2

main :: IO ()
main = solve . runParser parser <$> B.readFile "inputs/2023/input08.txt"
         >>= print
