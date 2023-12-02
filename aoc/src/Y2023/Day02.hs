{-# language OverloadedStrings #-}
{-# language TupleSections #-}
module Main ( main ) where 

import Utils ( runParser )
import Rec
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B

data Colors = Colors {_red :: Int, _green :: Int, _blue :: Int} deriving (Show, Eq)

combineColors :: [Colors] -> Colors
combineColors = cata alg . fromList
  where 
    alg NilF                                        = Colors 0 0 0
    alg (ConsF (Colors r1 g1 b1) (Colors r2 g2 b2)) = Colors (r1+r2) (g1+g2) (b1+b2)

parser :: Parser (Int, [Colors])
parser = do string "Game "
            n <- decimal
            string ": "
            (n,) <$> parseColors `sepBy` (string "; ")
  where 
      parseColors = combineColors <$> parseColor `sepBy` (string ", ")
      parseColor  = do n <- decimal
                       char ' '
                       getRed n <|> getGreen n <|> getBlue n 
      getRed n    = do string "red"
                       pure $ Colors n 0 0
      getGreen n  = do string "green"
                       pure $ Colors 0 n 0
      getBlue n   = do string "blue"
                       pure $ Colors 0 0 n

feasible :: Colors -> Bool
feasible (Colors r g b) = r <= 12 && g <= 13 && b <= 14 

maxCube :: Colors -> Colors -> Colors 
maxCube (Colors r1 g1 b1) (Colors r2 g2 b2) = Colors (max r1 r2) (max g1 g2) (max b1 b2)

getMinSet :: [Colors] -> Colors 
getMinSet = cata alg . fromList
  where
    alg NilF         = Colors 0 0 0
    alg (ConsF c cs) = maxCube c cs

power :: Colors -> Int 
power (Colors r g b) = r*g*b 

solve :: [(Int, [Colors])] -> (Int, Int)
solve = cata alg . fromList
  where
    alg NilF                      = (0, 0)
    alg (ConsF (ix, cs) (p1, p2)) = 
        ( p1 + if all feasible cs then ix else 0
        , p2 + power (getMinSet cs)
        )

main :: IO ()
main = solve . map (runParser parser) . B.lines <$> B.readFile "inputs/2023/input02.txt"
         >>= print
