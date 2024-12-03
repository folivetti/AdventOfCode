{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import Utils
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
import Replace.Attoparsec.ByteString
import qualified Data.ByteString.Char8 as B

parser = findAllCap parserMulB

parserMul :: Parser (Int, Int)
parserMul = do string "mul("
               v1 <- decimal
               char ','
               v2 <- decimal
               char ')'
               pure (v1,v2)

parseDo   = string "do()" >> pure True
parseDont = string "don't()" >> pure False

parserMulB :: Parser (Either Bool (Int, Int))
parserMulB = eitherP (parseDo <|> parseDont) parserMul

solve xs = go xs (0, 0) True
  where
    go [] v b = v
    go (Left _:ys) v b = go ys v b
    go (Right (_, Left b):ys) v _ = go ys v b
    go (Right (_, Right (a,c)):ys) (v1, v2) b = go ys (v1 + a*c, v2 + if b then a*c else 0) b
main :: IO ()
main = solve . runParser parser <$> B.readFile "inputs/2024/input03.txt"
        >>= print
