{-# language OverloadedStrings #-}
module Y2015.Day12 (solution) where

import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Char8 as P
import Control.Applicative ( (<|>) )

runParser :: Parser a -> B.ByteString -> a
runParser parser dat = case parse parser dat of
                  Done _ x -> x
                  Partial p -> case p "" of
                                 Done _ x -> x
                                 _ -> error "no parse"
                  _ -> error "no parse"
{-# INLINE runParser #-}

data JSON = Obj [(B.ByteString, JSON)]
          | SeqJ [JSON]
          | Number Int
          | StrJ B.ByteString
        deriving (Show, Eq)

whitespace = skipMany space

kvParse :: Parser (B.ByteString, JSON)
kvParse = do
    whitespace >> char '"' >> whitespace
    k <- P.takeWhile (/='"')
    whitespace >> string "\":" >> whitespace
    v <- parser
    pure (k, v)

parseObj :: Parser JSON
parseObj = do
  whitespace >> char '{' >> whitespace
  p <- sepBy kvParse (whitespace >> char ',' >> whitespace)
  whitespace >> char '}' >> whitespace
  pure $ Obj p

parseList :: Parser JSON
parseList = do
  whitespace >> char '[' >> whitespace
  p <- sepBy parser (whitespace >> char ',' >> whitespace)
  whitespace >> char ']' >> whitespace
  pure $ SeqJ p

parseNum :: Parser JSON
parseNum = Number <$> signed decimal

parseStr :: Parser JSON
parseStr = do
  whitespace >> char '"' >> whitespace
  s <- P.takeWhile (/='"')
  whitespace >> char '"' >> whitespace
  pure $ StrJ s

parser :: Parser JSON
parser = parseObj <|> parseList <|> parseStr <|> parseNum

getSum, getRedSum :: JSON -> Int
getSum (Obj kvs) = sum $ map (getSum . snd) kvs
getSum (SeqJ xs) = sum $ map getSum xs
getSum (Number x) = x
getSum _ = 0

getRedSum (Obj kvs) = let vs = map snd kvs
                      in if StrJ "red" `elem` vs
                           then 0
                           else sum $ map (getRedSum . snd) kvs
getRedSum (SeqJ xs) = sum $ map getRedSum xs
getRedSum (Number x) = x
getRedSum _ = 0


solution :: IO ()
solution = do
  content <- B.readFile "inputs/2015/input12.txt"
  let json = runParser parser content
  print $ getSum json
  print $ getRedSum json
