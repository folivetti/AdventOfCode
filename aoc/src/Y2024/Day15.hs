module Main where

import Utils
import Data.Maybe
import qualified Data.Map.Strict as Map
import Data.Bifunctor
import Control.Monad.State.Strict
import Control.Monad

type Coord = (Int, Int)

parse = second (filter (/='\n') . unlines . tail) . span (/="")
toMap = Map.fromList . filter ((/='.').snd) . toCoord

walk :: (Map.Map Coord Char, Coord) -> Char -> (Map.Map Coord Char, Coord)
walk (grid, pos) dir =
  case go Map.empty [pos] of
    Nothing   -> (grid, pos)
    Just upds -> (Map.union (Map.mapKeysMonotonic (`move` dir) upds) (Map.difference grid upds)
                 , move pos dir)
  where
    go :: Map.Map Coord Char -> [Coord] -> Maybe (Map.Map Coord Char)
    go upds [] = Just upds
    go upds (x:xs) =
      if x `Map.notMember` upds then
          case grid Map.!? x of
            Nothing  -> go upds xs
            Just '#' -> Nothing
            Just 'O' -> go (Map.insert x 'O' upds) (move x dir : xs)
            Just '@' -> go (Map.insert x '@' upds) (move x dir : xs)
            Just '[' -> go (Map.insert x '[' upds) $ if dir `elem` "^v" then (move x '>' : move x dir : xs) else (move x dir : xs)
            Just ']' -> go (Map.insert x ']' upds) $ if dir `elem` "^v" then (move x '<' : move x dir : xs) else (move x dir : xs)
            Just y   -> go (Map.insert x y upds) xs
    else go upds xs

move :: Coord -> Char -> Coord
move (x, y) '<' = (x, y-1)
move (x, y) '>' = (x, y+1)
move (x, y) '^' = (x-1, y)
move (x, y) 'v' = (x+1, y)


gps :: Map.Map Coord Char -> Int
gps = foldr (\(x, y) acc -> acc + x*100 + y) 0 . map fst . Map.toList . Map.filter (`elem` "O[")

--solve1 :: (Map.Map Coord Char, String) -> Int
solve1 (grid', moves) =  gps . fst $ foldl walk (grid, start) moves
  where
    start = fst . head . Map.toList $ Map.filter (=='@') grid
    grid  = toMap grid'

--solve2 :: (Map.Map Coord Char, String) -> Int
solve2 (grid', moves) =  gps . fst $ foldl walk (grid, start) moves
  where
    start = fst . head . Map.toList $ Map.filter (=='@') grid
    grid = toMap $ map expand grid'

expand :: String -> String
expand = concatMap double
  where
    double '#' = "##"
    double '@' = "@."
    double 'O' = "[]"
    double '.' = ".."

f &&& g = \x -> (f x, g x)

main :: IO ()
main = (solve1 &&& solve2) . parse . lines <$> readFile "inputs/2024/input15.txt"
         >>= print
