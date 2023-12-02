{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
module Main where

import Utils ( runParser )
import Data.Attoparsec.ByteString.Char8 ( anyChar, space, decimal, signed, string, Parser )
import qualified Data.ByteString.Char8 as B
import Control.Applicative ( (<|>) )
import Control.Lens ( (+~), (-~), makeLenses, (^.), (.~), (&) )

data Reg = Reg { _a :: Int
               , _b :: Int
               , _c :: Int
               , _d :: Int
               , _ix :: Int
               } deriving (Show, Eq)

makeLenses ''Reg

toAccess :: Functor f => Char -> (Int -> f Int) -> Reg -> f Reg
toAccess 'a' = a
toAccess 'b' = b
toAccess 'c' = c
toAccess 'd' = d
toAccess  x  = error $ "unknown reg " <> [x]

parseReg :: Functor f => Parser ( (Int -> f Int) -> Reg -> f Reg )
parseReg = toAccess <$> anyChar

parseCpy :: Parser (Reg -> Reg)
parseCpy = parseCpyVal <|> parseCpyReg
  where
    parseCpyVal = do string "cpy "
                     x <- signed decimal
                     space
                     r <- parseReg
                     pure $ (ix +~ 1) . (r .~ x)
    parseCpyReg = do string "cpy "
                     r1 <- parseReg
                     space
                     r2 <- parseReg
                     pure $ (ix +~ 1) . (\reg -> (r2 .~) (reg ^. r1) reg)

parseInc :: Parser (Reg -> Reg)
parseInc = do string "inc "
              r <- parseReg
              pure $ (ix +~ 1) . (r +~ 1)
parseDec :: Parser (Reg -> Reg)
parseDec = do string "dec "
              r <- parseReg
              pure $ (ix +~ 1) . (r -~ 1)

parseJnz :: Parser (Reg -> Reg)
parseJnz = parseJnzVal <|> parseJnzReg

parseJnzVal :: Parser (Reg -> Reg)
parseJnzVal = do string "jnz "
                 x <- signed decimal
                 space
                 y <- signed decimal
                 pure $ if x==0 then (ix +~ 1) else (ix +~ y)

parseJnzReg :: Parser (Reg -> Reg)
parseJnzReg = do string "jnz "
                 r <- parseReg
                 space
                 y <- signed decimal
                 pure $ \reg -> if (reg ^. r) == 0
                                  then (ix +~ 1) reg
                                  else (ix +~ y) reg

parser :: Parser (Reg -> Reg)
parser = parseCpy <|> parseInc <|> parseDec <|> parseJnz 

compute :: [B.ByteString] -> Reg -> Reg
compute instrs = go
  where
    n = length instrs
    go reg
      | reg ^. ix >= n = reg
      | otherwise      = go $ runParser parser (instrs !! _ix reg) reg

main :: IO ()
main = do content <- B.lines <$> B.readFile "inputs/2016/input12.txt"
          print $ compute content (Reg 0 0 0 0 0)
          print $ compute content (Reg 0 0 1 0 0)
