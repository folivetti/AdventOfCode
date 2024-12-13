{-# LANGUAGE OverloadedStrings #-}
module Main where

import Utils
import Data.List.Split
import Data.Maybe
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B

parseButton :: Parser (Int, Int)
parseButton = do string "Button "
                 string "A: " <|> string "B: "
                 string "X+"
                 x <- decimal
                 string ", Y+"
                 y <- decimal
                 char '\n'
                 pure (x, y)
parsePrize :: Parser (Int, Int)
parsePrize = do string "Prize: "
                string "X="
                x <- decimal
                string ", Y="
                y <- decimal
                char '\n'
                pure (x, y)

parseMachine :: Parser (Int, Int, Int, Int, Int, Int)
parseMachine = do (xa, ya) <- parseButton
                  (xb, yb) <- parseButton
                  (x,  y)  <- parsePrize
                  pure (xa, ya, xb, yb, x, y)

linalg :: (Int, Int, Int, Int, Int, Int) -> Maybe (Int, Int)
linalg (xa, ya, xb, yb, x, y)
  | r1 == 0 && r2 == 0 = Just (nx, ny)
  | otherwise          = Nothing
  where
    (ny, r1) = (ya * x - xa  * y) `quotRem` (ya * xb - xa * yb)
    (nx, r2) = (x - xb * ny) `quotRem` xa

cost nx ny = 3*nx+ny
f &&& g = \x -> (f x, g x)

adjust (xa, ya, xb, yb, x, y) = (xa, ya, xb, yb, x + 10000000000000, y + 10000000000000)

solve = both (foldr (\(nx, ny) acc -> cost nx ny + acc ) 0 . mapMaybe linalg) . (id &&& map adjust)

main :: IO ()
main = solve . map (runParser parseMachine . B.unlines) . chunksOf 3 . filter (/="") . B.lines <$> B.readFile "inputs/2024/input13.txt"
        >>= print
