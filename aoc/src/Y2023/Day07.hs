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
import Control.Arrow ( (&&&) )

parser :: Parser (String, Int)
parser = do hand <- many' (digit <|> letter_ascii)
            many' space 
            bid <- decimal
            pure (hand, bid)

cmpCards :: (String -> Int) -> String -> String -> String -> Ordering 
cmpCards getHand order x y = compare x' y'
  where
    x' = getHand x : map priority x
    y' = getHand y : map priority y

    priority c = fromJust (c `elemIndex` order)

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

typeOfHandJ :: String -> Int
typeOfHandJ card = maximum [typeOfHand (replace x card) | x <- nub card]
  where
    replace x = map (\y -> if y == 'J' then x else y) 

getBids getHand order = (`sumBids` 1) . fromList . hylo alg coalg
  where
    sumBids = accu st bids

    st NilF _         = NilF
    st (ConsF x xs) s = ConsF x (xs, s+1)

    bids NilF s             = 0
    bids (ConsF (_,b) xs) s = s*b + xs

    alg LeafF         = []
    alg (NodeF l x r) = l <> (x : r)

    coalg :: [(String, Int)] -> TreeF (String, Int) [(String, Int)]
    coalg []     = LeafF 
    coalg (x:xs) = NodeF lt x gt 
      where
        cmp = cmpCards getHand order (fst x)
        lt  = filter ((==GT) . cmp . fst) xs
        gt  = filter ((==LT) . cmp . fst) xs

part1 = getBids typeOfHand  "23456789TJQKA"
part2 = getBids typeOfHandJ "J23456789TQKA"

solve = part1 &&& part2

main :: IO ()
main = solve . map (runParser parser) . B.lines <$> B.readFile "inputs/2023/input07.txt"
         >>= print
