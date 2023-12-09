{-# language OverloadedStrings #-}
{-# Language QuasiQuotes, TransformListComp, ParallelListComp #-}
module Main ( main ) where 

import Utils ( runParser )
import Rec
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as Map
import Data.List ( sort, sortBy, elemIndex, nub )
import Data.Function ( on )
import Data.Maybe ( fromJust )

parser :: Parser (String, Int)
parser = do hand <- many' parseCard
            many' space 
            bid <- decimal
            pure (hand, bid)
  where
    parseCard = digit <|> letter_ascii

cmpCards :: String -> String -> Ordering 
cmpCards x y = compare x' y'
  where
    x' = typeOfHand x : map priority x
    y' = typeOfHand y : map priority y

    priority c = fromJust (c `elemIndex` "23456789TJQKA")

typeOfHand :: String -> Int
typeOfHand card  = 
    case sort (Map.elems m) of
      [5]          -> 6
      [1, 4]       -> 5
      [2, 3]       -> 4
      [1, 1, 3]    -> 3
      [1, 2, 2]    -> 2
      [1, 1, 1, 2] -> 1
      _            -> 0
  where
      m = Map.fromListWith (+) $ zip card (repeat 1)

cmpCards' :: String -> String -> Ordering 
cmpCards' x y = compare x' y'
  where 
    x' = typeOfHand' x : map priority x
    y' = typeOfHand' y : map priority y

    priority c = fromJust (c `elemIndex` "J23456789TQKA")

typeOfHand' :: String -> Int
typeOfHand' card = maximum [typeOfHand (replace x card) | x <- nub card]
  where
    replace x = map (\y -> if y == 'J' then x else y) 

solve input = 
    ( cata alg . fromList . zip [1 ..] . sortBy (cmpCards `on` fst) $ input
    , cata alg . fromList . zip [1 ..] . sortBy (cmpCards' `on` fst) $ input
    )
  where 
    alg NilF = 0
    alg (ConsF (r, (_, b)) xs) = r*b + xs 

main :: IO ()
main = solve . map (runParser parser) . B.lines <$> B.readFile "inputs/2023/input07.txt"
         >>= print
