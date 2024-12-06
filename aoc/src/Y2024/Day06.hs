{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import Utils
import Data.Set hiding (map,filter,elem)

data Direction = North | South | East | West deriving (Show, Eq, Ord)

type Coord = (Int, Int)

genMap :: [String] -> (Set Coord, (Coord, Direction), Coord)
genMap xss = ( fromList $ map (\((x,y),_) -> (x,y)) blocks
             , (fst guard, toDir (snd guard))
             , maxCoord
             )
  where
    coords = toCoord xss
    (maxCoord, _) = last coords
    blocks = filter (\((x, y), c) -> c=='#') coords
    guard  = head $ filter (\((x, y), c) -> c `elem` dirs) coords
    dirs = "v^<>" :: String
    toDir 'v' = South
    toDir '^' = North
    toDir '>' = East
    toDir '<' = West

move :: Set Coord -> (Coord, Direction) -> (Coord, Direction)
move blocks ((x, y), North) = if (x-1, y) `member` blocks
                                 then ((x, y), East)
                                 else ((x-1, y), North)
move blocks ((x, y), South) = if (x+1, y) `member` blocks
                                 then ((x, y), West)
                                 else ((x+1, y), South)
move blocks ((x, y), East) = if (x, y+1) `member` blocks
                                 then ((x, y), South)
                                 else ((x, y+1), East)
move blocks ((x, y), West) = if (x, y-1) `member` blocks
                                 then ((x, y), North)
                                 else ((x, y-1), West)

solve1 (blocks, guard, (maxX, maxY)) = size . fromList $ go (blocks, guard)
  where
    go :: (Set Coord, (Coord, Direction)) -> [Coord]
    go (blocks, guard)
      | isInside (fst guard) = fst guard : go (blocks, move blocks guard)
      | otherwise = []

    isInside (x, y) = x >= 0 && y >= 0 && x <= maxX && y <= maxY

solve2 (blocks, guard, (maxX, maxY)) = length $ filter (hasLoop empty guard) $ map (`insert` blocks) emptyBlocks
  where
    emptyBlocks = [(x, y) | x <- [0..maxX], y <- [0..maxY], (x,y) /= fst guard, (x,y) `notElem` blocks]
    isInside (x, y) = x >= 0 && y >= 0 && x <= maxX && y <= maxY

    hasLoop visited grd@((x,y), dir) blocks'
      | grd `member` visited = True
      | isInside (x, y) = hasLoop (insert grd visited) (move blocks' grd) blocks'
      | otherwise = False

solve x = (solve1 x, solve2 x)

main :: IO ()
main = solve . genMap . lines <$> readFile "inputs/2024/input06.txt"
        >>= print
