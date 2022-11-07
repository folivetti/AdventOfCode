{-# LANGUAGE OverloadedStrings #-}
module Y2015.Day07 (solution) where

import Utils ( runParser )
import Data.Bits ( Bits(complement, (.&.), (.|.), shiftR, shiftL) )
import Data.Word ( Word16 )
import qualified Data.Map as M

import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8 ( decimal, many', string, anyChar, manyTill, Parser )
import Data.Attoparsec.ByteString.Char8 as P ( takeWhile )
import Control.Applicative ( (<|>) )

parserDest :: Parser String
parserDest =  manyTill anyChar (string "-> ")
           >> many' anyChar
{-# INLINE parserDest #-}

fromPos :: M.Map String Int -> B.ByteString -> Int
fromPos table = (table M.!) . B.unpack
{-# INLINE fromPos #-}

parserInst :: M.Map String Int -> Parser ([Word16] -> Word16)
parserInst table = do
  f <- parseFun table
  _ <- many' anyChar
  pure f

parseFun :: M.Map String Int -> Parser ([Word16] -> Word16)
parseFun table =  parseOp table
              <|> parseShift table
              <|> parseNot table
              <|> parseAssign table

parseIx :: M.Map String Int -> Parser ([Word16] -> Word16)
parseIx table = do
  ix <- fromPos table <$> P.takeWhile (/=' ')
  pure (!! ix)

parseConst :: Parser ([Word16] -> Word16)
parseConst = const <$> decimal

parseAssign :: M.Map String Int -> Parser ([Word16] -> Word16)
parseAssign table = parseConst <|> parseIx table

parseOp :: M.Map String Int -> Parser ([Word16] -> Word16)
parseOp table = do
  i1 <- parseConst <|> parseIx table
  op <- parseAnd <|> parseOr
  i2 <- parseConst <|> parseIx table
  pure $ \xs -> op (i1 xs) (i2 xs)
      where 
          parseAnd = string " AND " >> pure (.&.)
          parseOr  = string " OR " >> pure (.|.)

parseShift :: M.Map String Int -> Parser ([Word16] -> Word16)
parseShift table = do
    i1 <- parseIx table
    f  <- parseRShift <|> parseLShift
    x  <- decimal
    pure $ \xs -> f (i1 xs) x
  where
      parseRShift = string " RSHIFT " >> pure shiftR
      parseLShift = string " LSHIFT " >> pure shiftL

parseNot :: M.Map String Int -> Parser ([Word16] -> Word16)
parseNot table = do
  _ <- string "NOT "
  i1 <- parseIx table
  pure $ \xs -> complement (i1 xs)

createMap :: [String] -> M.Map String Int
createMap xs = M.fromList $ zip xs [0..]
{-# INLINE createMap #-}

loeb :: Functor f => f (f a -> a) -> f a
loeb x = go where go = fmap ($ go) x

solution :: IO ()
solution = do
  content <- B.lines <$> B.readFile "inputs/2015/input07.txt"
  let table = createMap $ map (runParser parserDest) content
      instrs = map (runParser (parserInst table)) content
      res = loeb instrs
      ixa = table M.! "a"
      ixb = table M.! "b"
      part1 = res !! ixa
      override = Prelude.take ixb instrs <> [const part1] <> Prelude.drop (ixb+1) instrs
      part2 = loeb override !! ixa
  print part1
  print part2
