{-# language OverloadedStrings #-}
module Y2015.Day08 (solution) where

import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8 
import Control.Applicative ( (<|>) )

parseLen :: Parser Int
parseLen = do
  _ <- char '"'
  parseInnerLen
  where
    parseEscape = char '\\' >> anyChar >> pure 1
    parseHex = char '\\' >> char 'x' >> anyChar >> anyChar >> pure 1

    parseInnerLen = do
      mc <- peekChar
      case mc of
        Nothing -> pure 0
        Just '"' -> pure 0
        _ -> do nxt <- parseHex <|> parseEscape <|> (anyChar >> pure 1)
                x <- parseInnerLen
                pure (x+nxt)

parseStr :: Parser Int
parseStr = do
  _ <- char '"'
  (+3) <$> parseInnerLen
  where
    parseEscape = char '\\' >> anyChar >> pure 4
    parseHex = char '\\' >> char 'x' >> anyChar >> anyChar >> pure 5

    parseInnerLen = do
      mc <- peekChar
      case mc of
        Nothing -> pure 0
        Just '"' -> pure 3
        _ -> do nxt <- parseHex <|> parseEscape <|> (anyChar >> pure 1)
                x <- parseInnerLen
                pure (x+nxt)
      
runParser :: Parser a -> B.ByteString -> a
runParser parser dat = case parse parser dat of
                  Done _ x -> x
                  Partial p -> case p "" of
                                 Done _ x -> x
                                 _ -> error "no parse"
                  _ -> error "no parse"
{-# INLINE runParser #-}

solution :: IO ()
solution = do
  content <- B.lines <$> B.readFile "inputs/2015/input08.txt"
  let 
    mem1 = map (runParser parseLen) content
    mem2 = map (runParser parseStr) content
    len = map B.length content
    part1 = sum len - sum mem1
    part2 = sum mem2 - sum len
  print part1
  print part2
