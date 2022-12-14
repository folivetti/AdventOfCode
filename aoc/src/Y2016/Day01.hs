{-# language OverloadedStrings #-}
module Main where

import Utils ( runParser )
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8 ( decimal, char, string, Parser, sepBy )
import Control.Applicative ( (<|>) )
import Data.Bifunctor ( second, bimap )
import qualified Data.Map.Strict as M

type Pos = (Int, Int)
type Move = (Pos, Pos) -> [(Pos, Pos)]

parseFuns :: Parser [Move]
parseFuns = parseToFun `sepBy` string ", "

parseToFun :: Parser Move
parseToFun = parse 'L' toLeft <|> parse 'R' toRight

toLeft, toRight :: Pos -> Pos
toLeft (x, y)  = (-y, x)
toRight (x, y) = (y, -x)

add :: (Pos, Pos) -> (Pos, Pos)
add ((a, b), (c, d)) = ((a + c, b + d), (c, d))

parse :: Char -> (Pos -> Pos) -> Parser Move
parse c f = do char c
               n <- decimal
               pure $ take n . tail . iterate add . second f

s0 :: (Pos, Pos)
s0 = ((0, 0), (0, 1))

firstRepeat :: [(Int, Int)] -> Int
firstRepeat = manhattan . go M.empty
  where
    go :: M.Map (Int, Int) Int -> [(Int, Int)] -> (Int, Int)
    go _ []     = error "no repeating coordinate"
    go _ [s]    = s
    go myDict (s:ss)
      | myDict' M.!? s >= Just 2  = s
      | otherwise                 = go myDict' ss
      where
        myDict' = M.insertWith (+) s 1 myDict

manhattan :: (Int, Int) -> Int
manhattan = uncurry (+) . bimap abs abs

walk :: [(Pos, Pos) -> [(Pos, Pos)]] -> [(Pos, Pos)]
walk = concat . scanl (flip ($) . last) [s0]

main :: IO ()
main = do
    funs <- runParser parseFuns <$> B.readFile "inputs/2016/input01.txt"
    let coords = walk funs
    print $ (manhattan . fst . last) coords 
    print $ (firstRepeat . map fst) coords
