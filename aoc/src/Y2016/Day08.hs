{-# language OverloadedStrings #-}
module Main where

import Utils ( runParser )
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.Array.IO as IA
import Data.Foldable ( traverse_ )
import Control.Applicative ( (<|>) )
import Control.Monad ( foldM, forM_ )

type Arr = IA.IOUArray (Int, Int) Int

parser :: Parser (Arr -> IO ())
parser = parseRect <|> parseRotateRow <|> parseRotateCol
  where
    parseRect = do string "rect "
                   x <- decimal
                   char 'x'
                   rect x <$> decimal

    parseRotateRow = do string "rotate row y="
                        x <- decimal
                        string " by "
                        rotateRow x <$> decimal

    parseRotateCol = do string "rotate column x="
                        x <- decimal
                        string " by "
                        rotateCol x <$> decimal

rect :: Int -> Int -> Arr -> IO ()
rect x y arr = traverse_ (\xy -> IA.writeArray arr xy 1) [(a, b) | a <- [0..y-1], b <- [0..x-1]]

rotateRow, rotateCol :: Int -> Int -> Arr -> IO ()
rotateRow ix x arr = forM_ [1 .. x] $ \_ ->
                     do y <- IA.readArray arr (ix, 49)
                        traverse_ (\j -> IA.readArray arr (ix, j) >>= IA.writeArray arr (ix, j+1)) [48, 47 .. 0]
                        IA.writeArray arr (ix, 0) y
rotateCol iy x arr = forM_ [1 .. x] $ \_ ->
                     do y <- IA.readArray arr (5, iy)
                        traverse_ (\i -> IA.readArray arr (i, iy) >>= IA.writeArray arr (i+1, iy)) [4,3 .. 0]
                        IA.writeArray arr (0, iy) y

solvePart1 :: [Arr -> IO ()] -> IO Int
solvePart1 instrs = do
  arr <- IA.newArray ((0,0), (5,49)) 0 :: IO Arr
  traverse_ ($ arr) instrs
  forM_ [0 .. 5] $ \x ->
      do forM_ [0 .. 49] $ \y -> do
            z <- IA.readArray arr (x,y)
            if z == 0
              then putChar ' '
              else putChar '#'
         putChar '\n'
  foldM (\acc xy -> (acc +) <$> IA.readArray arr xy) 0 [(x,y) | x <- [0 .. 5], y <- [0 .. 49]]


main :: IO ()
main = do content <- B.lines <$> B.readFile "inputs/2016/input08.txt" 
          let funs = map (runParser parser) content
          part1 <- solvePart1 funs
          print part1
