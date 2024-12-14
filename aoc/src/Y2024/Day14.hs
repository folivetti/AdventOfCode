{-# LANGUAGE OverloadedStrings #-}
module Main where

import Utils
import Data.Maybe
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad

data Quad = Q1 | Q2 | Q3 | Q4 | Middle deriving (Eq, Ord, Bounded, Enum)

parseRobots :: Parser ((Int, Int), (Int, Int))
parseRobots = do
  string "p="
  px <- signed decimal
  string ","
  py <- signed decimal
  string " v="
  vx <- signed decimal
  string ","
  vy <- signed decimal
  pure ((px, py), (vx, vy))

boundX = 101 -- 11
boundY = 103 -- 7



half = (`div` 2)

toQuad (x, y)
  | x < half boundX && y < half boundY = Q1
  | x > half boundX && y < half boundY = Q2
  | x < half boundX && y > half boundY = Q3
  | x > half boundX && y > half boundY = Q4
  | otherwise = Middle

walk :: Int -> (Int, Int) -> (Int, Int) -> (Int, Int)
walk t (px, py) (vx, vy) = ((px + t*vx) `mod` boundX, (py + t*vy) `mod` boundY)

countWith f = Map.toList . Map.fromListWith (+) . map (\x -> (f x, 1))

quadMap = map snd . filter ((/=Middle).fst) . countWith toQuad -- Map.fromListWith (+) . map (\x -> (toQuad x, 1))

solve1 = product . quadMap . map (uncurry (walk 100))

solve2 robots = go 10000 $ map fst robots
  where
    step = uncurry (walk 1)
    vel = map snd robots

    go 0 pos' = pure ()
    go n pos' = do let pos = map step (zip pos' vel)
                   when (any ((>20).snd) (countWith fst pos)
                            && any ((>20).snd) (countWith snd pos)) $ printRobots (10000-n+1) pos
                   go (n-1) pos
printRobots it pos = do
  let spos = Set.fromList pos
  print it
  putStrLn $ unlines [[if (x, y) `Set.member` spos then '#' else '.' | x <- [0..boundX]] | y <- [0..boundY]]

main :: IO ()
main = do robots <- map (runParser parseRobots) . B.lines <$> B.readFile "inputs/2024/input14.txt"
          print $ solve1 robots
          solve2 robots
