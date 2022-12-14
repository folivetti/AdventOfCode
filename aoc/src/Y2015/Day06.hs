{-# language OverloadedStrings #-}
module Main where

import Utils ( runParser )

import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8 ( char, decimal, string, Parser )
import Control.Applicative ( (<|>) )
import qualified Data.Array.IO as IA
import Data.Array.Base ( MArray(unsafeWrite, unsafeRead) )
import Control.Monad ( forM_ )

data Op = Off | On | Toggle 

getOp :: B.ByteString -> Op
getOp "turn on " = On
getOp "turn off " = Off
getOp "toggle " = Toggle
getOp s = error $ B.unpack s
{-# INLINE getOp #-}

parser :: Parser (Op, [Int])
parser = do
    op <- string "turn on " <|> string "turn off " <|> string "toggle "
    x1 <- decimal
    _ <- char ','
    y1 <- decimal
    _ <- string " through "
    x2 <- decimal
    _ <- char ','
    y2 <- decimal
    pure (getOp op, [a*1000 + b | a <- [x1 .. x2], b <- [y1 .. y2]])
{-# INLINE parser #-}

type Arr = IA.IOUArray Int Int

solveAll :: [B.ByteString] -> IO (Int, Int)
solveAll instrs = do
  arr <- IA.newArray (0, 1000001) 0 :: IO Arr
  forM_ instrs (writeRng arr . runParser parser)
  getAns arr
  x1 <- unsafeRead arr 1000000
  x2 <- unsafeRead arr 1000001
  pure (x1, x2)
  where
      getAns arr = forM_ [0 .. 999999] $ \ix -> do
        x1 <- unsafeRead arr 1000000
        x2 <- unsafeRead arr 1000001
        x  <- unsafeRead arr ix
        let (a, b) = x `quotRem` 10
        unsafeWrite arr 1000000 (x1 + b)
        unsafeWrite arr 1000001 (x2 + a)

      writeRng :: Arr -> (Op, [Int]) -> IO ()
      writeRng arr (On, xs)     = forM_ xs (readWrite arr fOn)
      writeRng arr (Off, xs)    = forM_ xs (readWrite arr fOff)
      writeRng arr (Toggle, xs) = forM_ xs (readWrite arr fToggle)

      readWrite arr f c = do
        b <- unsafeRead arr c
        unsafeWrite arr c (f b)

      fOn x     = (x `quot` 10 + 1) * 10 + 1
      fOff x    = max 0 (x `quot` 10 - 1) * 10
      fToggle x = (x `quot` 10 + 2) * 10 + (1 - x `rem` 10)

main :: IO ()
main = do
  content <- B.lines <$> B.readFile "inputs/2015/input06.txt"
  p1 <- solveAll content
  print p1 
