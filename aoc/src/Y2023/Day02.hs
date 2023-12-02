{-# language OverloadedStrings #-}
module Main ( main ) where 

import Utils ( runParser )
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B

data Colors = Colors {_red :: Int, _green :: Int, _blue :: Int} deriving (Show, Eq)

instance Semigroup Colors where 
    Colors r1 g1 b1 <> Colors r2 g2 b2 = Colors (r1+r2) (g1+g2) (b1+b2) 

instance Monoid Colors where 
    mempty = Colors 0 0 0 

instance Ord Colors where 
    Colors r1 g1 b1 <= Colors r2 g2 b2 = r1 <= r2 && g1 <= g2 && b1 <= b2

parser :: Parser [Colors]
parser = do string "Game "
            decimal
            string ": "
            parseColors `sepBy` (string "; ")
  where 
      parseColors = mconcat <$> parseColor `sepBy` (string ", ")
      parseColor  = do n <- decimal
                       char ' '
                       getRed n <|> getGreen n <|> getBlue n 
      getRed n    = do string "red"
                       pure $ mempty{_red = n}
      getBlue n   = do string "blue"
                       pure $ mempty{_blue = n}
      getGreen n  = do string "green"
                       pure $ mempty{_green = n}

maxCube :: Colors -> Colors -> Colors 
maxCube (Colors r1 g1 b1) (Colors r2 g2 b2) = Colors (max r1 r2) (max g1 g2) (max b1 b2)

getMinSet :: [Colors] -> Colors 
getMinSet = foldr maxCube mempty

power :: Colors -> Int 
power (Colors r g b) = r*g*b 

getPow :: (Int, [Colors]) -> Int
getPow = power . getMinSet . snd 

main :: IO ()
main = do content <- B.lines <$> B.readFile "inputs/2023/input02.txt"
          let myDat      = zip [1..] $ map (runParser parser) content 
              isPossible = all (<= Colors 12 13 14) . snd
          print $ foldr (\c acc -> if isPossible c then fst c + acc else acc) 0 myDat
          print $ foldr (\c acc -> getPow c + acc) 0 myDat
