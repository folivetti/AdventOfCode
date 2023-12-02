{-# language OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Char8 as B
import Data.Bifunctor ( second, bimap )

splitOn :: Char -> B.ByteString -> (B.ByteString, B.ByteString)
splitOn c = second safeTail . B.span (/=c)

safeTail :: B.ByteString -> B.ByteString
safeTail xs
  | B.null xs = xs
  | otherwise = B.tail xs

parseInstructions :: B.ByteString -> (Int, Int, B.ByteString)
parseInstructions instr = let (numbers, next) = splitOn ')' instr
                              (m, n)          = bimap bRead bRead $ splitOn 'x' numbers
                           in (m, n, next)
                          where bRead = read . B.unpack

process :: Bool -> B.ByteString -> Int
process ver2 xs 
  | B.null xs    = 0
  | B.null instr = B.length prev
  | otherwise    = if ver2
                      then B.length prev + n * process ver2 rep + process ver2 next
                      else B.length prev + (m*n) + process ver2 next
  where
    (prev, instr) = splitOn '(' xs
    (m, n, block) = parseInstructions instr
    (rep, next)   = B.splitAt m block

main :: IO ()
main = do content <- B.lines <$> B.readFile "inputs/2016/input09.txt"
          print $ sum $ map (process False) content
          print $ sum $ map (process True) content
