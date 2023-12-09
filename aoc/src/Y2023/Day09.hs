{-# language OverloadedStrings #-}
module Main ( main ) where 

import Utils ( runParser )
import Rec
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.Set as Set 
import qualified Data.Map.Strict as Map

parser :: Parser [Int]
parser = (signed decimal) `sepBy` space 

step []       = []
step [x]      = []
step (x:y:xs) = (y-x) : step (y:xs)

part1 = sum . map last . Prelude.takeWhile (not . all (==0)) . iterate step
part2 = foldr (\x acc -> x - acc) 0 . map head . Prelude.takeWhile (not . all (==0)) . iterate step

solve = sum . map part2

main :: IO ()
main = solve . map (runParser parser) . B.lines <$> B.readFile "inputs/2023/input09.txt"
         >>= print
