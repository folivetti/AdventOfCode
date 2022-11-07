{-# language OverloadedStrings #-}
module Y2015.Day18 (solution) where

import Utils ( runParser )
import Data.Attoparsec.ByteString.Char8 ( char, endOfLine, many', sepBy, Parser )
import qualified Data.ByteString.Char8 as B
import Control.Applicative ((<|>))
import Data.Maybe (mapMaybe)
import qualified Data.Array.IO as IA
import Data.Array.Base ( MArray(unsafeWrite, unsafeRead) )
import Control.Monad ( forM_, forM, replicateM_ )

parseChars :: Parser Char
parseChars = char '#' <|> char '.'

createSet :: [String] -> [(Int, Int)]
createSet = concat . zipWith toCoord [0..]
  where
    toCoord x           = mapMaybe (maybeCoord x) . zip [0..]
    maybeCoord x (y, c) = if c=='#' then Just (x,y) else Nothing

myParser :: Parser [(Int, Int)]
myParser = do
    charMap <- many' parseChars `sepBy` endOfLine
    pure $ createSet charMap

type Arr = IA.IOUArray Int Int

runUntil :: Int -> [(Int, Int)] -> IO Int
runUntil it start = do
  arr <- IA.newArray (0, 9999) 0 :: IO Arr
  forM_ start (turnOn arr)
  replicateM_ it (step arr)
  sum <$> forM [0 .. 9999] (unsafeRead arr)

runUntil2 :: Int -> [(Int, Int)] -> IO Int
runUntil2 it start = do
  arr <- IA.newArray (0, 9999) 0 :: IO Arr
  forM_ start (turnOn arr)
  replicateM_ it (turnCorners arr >> step arr >> turnCorners arr)
  sum <$> forM [0 .. 9999] (unsafeRead arr)

turnCorners :: Arr -> IO ()
turnCorners arr = do
    turnOn arr (0, 0)
    turnOn arr (0, 99)
    turnOn arr (99, 0)
    turnOn arr (99, 99)

turnOn, turnOff :: Arr -> (Int, Int) -> IO ()
turnOn arr (x, y)  = unsafeWrite arr (x*100 + y) 1
turnOff arr (x, y) = unsafeWrite arr (x*100 + y) 0

getVal :: Arr -> (Int, Int) -> IO Int
getVal arr (x, y)  = unsafeRead arr (x*100 + y)

step :: Arr -> IO ()
step arr = do funs <- concat <$> forM coords (applyRule arr)
              mapM_ ($ arr) funs

coords :: [(Int, Int)]
coords = (,) <$> [0 .. 99] <*> [0 .. 99]

onNeighs :: Arr -> (Int, Int) -> IO Int
onNeighs arr xy = do vals <- forM (neigh xy) (getVal arr)
                     pure (length . filter (==1) $ vals)

valid :: (Int, Int) -> Bool
valid (x, y)       = x >= 0 && x < 100 && y >= 0 && y < 100

neigh :: (Int, Int) -> [(Int, Int)]
neigh (x, y)       = filter valid
                   $ filter (/=(x,y))
                   $ (,) <$> [x-1 .. x+1] <*> [y-1 .. y+1]

applyRule :: Arr -> (Int, Int) -> IO [Arr -> IO ()]
applyRule arr xy = do
    v       <- getVal arr xy
    lighted <- onNeighs arr xy
    if v == 1 && lighted /= 2 && lighted /= 3
       then pure [(`turnOff` xy)]
       else if v == 0 && lighted == 3
              then pure [(`turnOn` xy)]
              else pure []

solution :: IO ()
solution = do
  content <- runParser myParser <$> B.readFile "inputs/2015/input18.txt"
  n1 <- runUntil 100 content
  n2 <- runUntil2 100 content
  print n1
  print n2
