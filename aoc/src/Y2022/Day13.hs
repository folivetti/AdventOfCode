{-# language OverloadedStrings #-}
module Y2022.Day13 ( solution ) where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import Control.Applicative ( (<|>) )
import Data.List ( sort, elemIndex, groupBy )
import Utils ( runParser )

data Packet = Lit Int | List [Packet] deriving (Show, Eq)

instance Ord Packet where
  compare (Lit x) (Lit y)             = compare x y
  compare (List (x:xs)) (List (y:ys)) = compare x y <> compare (List xs) (List ys)
  compare (List []) (List [])         = EQ
  compare (List []) (List _)          = LT
  compare (List _) (List [])          = GT
  compare x@(Lit _) y                 = compare (List [x]) y
  compare x y@(Lit _)                 = compare x (List [y])

parser :: Parser Packet
parser =  Lit <$> decimal 
      <|> List <$> do char '['
                      xs <- parser `sepBy` char ','
                      char ']'
                      pure xs

toPairs :: [a] -> (a, a)
toPairs [x, y] = (x, y)
toPairs _      = error "wrong size of a list"

groupPairs :: [B.ByteString] -> [[B.ByteString]]
groupPairs = map (filter (/="")) . groupBy (\_ y -> y /= "") 

part1 :: [[Packet]] -> Int
part1 = sum . map fst . filter (uncurry (<) . snd) . zip [1..] . map toPairs

part2 :: [[Packet]] -> Maybe Int
part2 = multIdx . sort . ([div1, div2] <>) . concat
  where
    div1       = List [List [Lit 6]]
    div2       = List [List [Lit 2]]
    mulOne x y = (x+1) * (y+1)
    multIdx xs = mulOne <$> elemIndex div1 xs <*> elemIndex div2 xs

solution :: IO ()
solution = do content <- groupPairs . B.lines <$> B.readFile "inputs/2022/input13.txt"
              let pairs = map (map (runParser parser)) content
              print $ part1 pairs
              print $ part2 pairs
