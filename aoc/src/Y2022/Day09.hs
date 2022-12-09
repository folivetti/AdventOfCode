module Y2022.Day09 ( solution ) where

import Data.Bifunctor ( bimap, first, second )
import Data.Set ( fromList, size )

type Coord = (Int, Int)

moveTail :: Coord -> Coord -> Coord
moveTail (x, y) t@(a, b)
  | t `elem` neighbors = t
  | otherwise          = (a + signum (x - a), b + signum (y - b))
  where
    neighbors = [(x+dx, y+dy) | dx <- [-1 .. 1], dy <- [-1 .. 1]]

moveHead :: String -> Coord -> Coord
moveHead "U" = first pred
moveHead "D" = first succ
moveHead "R" = second succ
moveHead "L" = second pred
moveHead _   = error "unknown direction"

move :: (Coord -> Coord) -> [Coord] -> [Coord]
move _ []     = error "empty coords"
move f (x:xs) = go (f x) xs
  where
    go y []     = [y]
    go y (z:zs) = y : go (moveTail y z) zs

followInstruction :: [String] -> [Coord] -> [[Coord]]
followInstruction [i, x] = take (read x) . tail . iterate (move (moveHead i))
followInstruction _ = error "invalid instruction"

snake :: [[String]] -> [Coord] -> [[Coord]]
snake [] _ = []
snake (x:xs) knots = knots' <> snake xs (last knots')
  where knots' = followInstruction x knots

solution :: IO ()
solution = do content <- map words . lines <$> readFile "inputs/2022/input09.txt"
              let solve = size . fromList . map last . snake content
              print $ solve [(0,0), (0,0)]
              print $ solve $ replicate 10 (0,0)
