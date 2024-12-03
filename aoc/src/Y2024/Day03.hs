{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import Utils
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
import Replace.Attoparsec.ByteString
import qualified Data.ByteString.Char8 as B

parser :: Parser [Either B.ByteString (B.ByteString, (Int, Int))]
parser = findAllCap parserMul

parser2 = findAllCap parserMulB

parserMul :: Parser (Int, Int)
parserMul = do string "mul("
               v1 <- decimal
               char ','
               v2 <- decimal
               char ')'
               pure (v1,v2)

parseDo = string "do()" >> pure True
parseDont = string "don't()" >> pure False

parserMulB :: Parser (Either Bool (Int, Int))
parserMulB = eitherP (parseDo <|> parseDont) parserMul

solve xs = go xs 0
  where
    go [] v = v
    go (Left _:ys) v = go ys v
    go (Right (_, (a,b)):ys) v = go ys (v + a*b)
solve2 xs = go xs 0 True
  where
    go [] v b = v
    go (Left _:ys) v b = go ys v b
    go (Right (_, Left b):ys) v _ = go ys v b--(if b then v + a*c else v) b
    go (Right (_, Right (a,c)):ys) v b = go ys (if b then v + a*c else v) b
main :: IO ()
main = do inp <- B.readFile "inputs/2024/input03.txt"
          print $ (solve . runParser parser) inp
          print $ (solve2 . runParser parser2) inp
