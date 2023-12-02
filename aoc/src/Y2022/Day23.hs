module Main where

import qualified Data.Set as S
import Data.List ( findIndex )

type Coord = (Int, Int)
type Neighbors = [[Coord]]
type Elves = S.Set Coord

neighbors :: Neighbors
neighbors = [ [(-1, 0), (-1, -1), (-1, 1)]
            , [(1, 0), (1, -1), (1, 1)]
            , [(0, -1), (-1, -1), (1, -1)]
            , [(0, 1), (-1, 1), (1, 1)]
            ]

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

add :: Coord -> Coord -> Coord
add (x, y) (a, b) = (x+a, y+b)

move :: Neighbors -> Elves -> Coord -> Coord
move neighs elves elf 
  | anybodyThere = safeHead elf next
  | otherwise    = elf
  where
    neigh = take 4 neighs
    anybodyThere  = any (`S.member` elves) $ concatMap (map (add elf)) neigh
    safeHead x [] = x
    safeHead _ xs = head xs
    next = [ head places | n <- neigh
                         , let places = map (add elf) n
                         , all (`S.notMember` elves) places 
           ]

step :: (Elves, Neighbors) -> (Elves, Neighbors)
step (elves, neighs) = (elves', tail neighs)
  where
    elves' = S.foldl' makeMove S.empty elves
    makeMove acc elf
      | elf' `S.member` acc = S.insert (counter elf elf') $ S.insert elf $ S.delete elf' acc
      | otherwise           = S.insert elf' acc
      where elf' = move neighs elves elf
            counter (x, y) (a, b) = (2*a - x, 2*b - y)

getRekt :: Elves -> Int
getRekt = area . S.foldl' updateCount (0, maxBound, maxBound, minBound, minBound)
  where
    area (c, minX, minY, maxX, maxY) = (maxX - minX + 1)*(maxY - minY + 1) - c
    updateCount (c, minX, minY, maxX, maxY) (x, y) = (c+1, min minX x, min minY y, max maxX x, max maxY y)

main :: IO ()
main = do content <- lines <$> readFile "inputs/2022/input23.txt"
          let elves = S.fromList [ (r, c) | (r, row) <- enumerate content
                                          , (c, elf) <- enumerate row
                                          , elf == '#'
                                 ]
              states = map fst $ iterate step (elves, cycle neighbors)
          print $ getRekt $ states !! 10
          print $ (+1) <$> findIndex (uncurry (==)) (zip states $ tail states)
