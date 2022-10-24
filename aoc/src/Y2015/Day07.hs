{-# LANGUAGE OverloadedStrings #-}
module Y2015.Day07 (solution) where

import Data.Bits
import Data.Word
import qualified Data.Map as M

import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.ByteString.Char8 as P
import Control.Applicative ( (<|>) )

parserDest :: Parser String
parserDest = do
    _ <- manyTill anyChar (string "-> ")
    many' anyChar
{-# INLINE parserDest #-}

getIx :: Int -> [a] -> a
getIx ix xs = xs !! ix
{-# INLINE getIx #-}

fromPos :: M.Map String Int -> B.ByteString -> Int
fromPos table = (table M.!) . B.unpack
{-# INLINE fromPos #-}

parserInst :: M.Map String Int -> Parser ([Word16] -> Word16)
parserInst table =  parse1And table
                <|> parseAnd table
                <|> parseOr table
                <|> parseNot table
                <|> parseRShift table
                <|> parseLShift table
                <|> parseAssign table


parseAssign :: M.Map String Int -> Parser ([Word16] -> Word16)
parseAssign table = do
  f <- (const <$> decimal) <|> (getIx . fromPos table <$> P.takeWhile (/=' '))
  _ <- many' anyChar
  pure f

parse1And :: M.Map String Int -> Parser ([Word16] -> Word16)
parse1And table = do
  x <- decimal
  _ <- string " AND "
  i2 <- fromPos table <$> P.takeWhile (/=' ')
  _ <- many' anyChar
  pure $ \xs -> x .&. (xs !! i2)

parseAnd :: M.Map String Int -> Parser ([Word16] -> Word16)
parseAnd table = do
  i1 <- (table M.!) . B.unpack <$> P.takeWhile (/=' ')
  _ <- string " AND "
  i2 <- (table M.!) . B.unpack <$> P.takeWhile (/=' ')
  _ <- many' anyChar
  pure $ \xs -> (xs !! i1) .&. (xs !! i2)

parseOr :: M.Map String Int -> Parser ([Word16] -> Word16)
parseOr table = do
  i1 <- (table M.!) . B.unpack <$> P.takeWhile (/=' ')
  _ <- string " OR "
  i2 <- (table M.!) . B.unpack <$> P.takeWhile (/=' ')
  _ <- many' anyChar
  pure $ \xs -> (xs !! i1) .|. (xs !! i2)

parseRShift :: M.Map String Int -> Parser ([Word16] -> Word16)
parseRShift table = do
  i1 <- (table M.!) . B.unpack <$> P.takeWhile (/=' ')
  _ <- string " RSHIFT "
  i2 <- decimal
  _ <- many' anyChar
  pure $ \xs -> shiftR (xs !! i1) i2

parseLShift :: M.Map String Int -> Parser ([Word16] -> Word16)
parseLShift table = do
  i1 <- (table M.!) . B.unpack <$> P.takeWhile (/=' ')
  _ <- string " LSHIFT "
  i2 <- decimal
  _ <- many' anyChar
  pure $ \xs -> shiftL (xs !! i1) i2

parseNot :: M.Map String Int -> Parser ([Word16] -> Word16)
parseNot table = do
  _ <- string "NOT "
  i1 <- (table M.!) . B.unpack <$> P.takeWhile (/=' ')
  _ <- many' anyChar
  pure $ \xs -> complement (xs !! i1)

runParser :: Parser a -> B.ByteString -> a
runParser parser dat = case parse parser dat of
                  Done _ x -> x
                  Partial p -> case p "" of
                                 Done _ x -> x
                                 _ -> error "no parse"
                  _ -> error "no parse"
{-# INLINE runParser #-}

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
