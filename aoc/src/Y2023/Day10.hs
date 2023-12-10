module Main ( main ) where 

import Utils ( toCoord )
import Rec
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type Coord = (Int, Int) 
type Pipelines = Map.Map Coord [Coord]

goUp (x, y)    = (x-1, y)
goDown (x, y)  = (x+1, y)
goLeft (x, y)  = (x, y-1)
goRight (x, y) = (x, y+1)

allDirs = [goUp, goLeft, goDown, goRight]

parser xs = (s0, fixed)
  where
    pipelines = Map.fromList $ map toPipe $ toCoord xs
    pipes     = toCoord xs
    s0        = fst . head $ filter ((=='S') . snd) pipes
    fixed     = let coords = [f s0 | f <- allDirs, s0 `elem` (pipelines Map.! (f s0))]
                 in Map.insert s0 coords pipelines

    toPipe ((x,y), '|') = ((x,y), [(x-1,y), (x+1,y)])
    toPipe ((x,y), '-') = ((x,y), [(x,y-1), (x,y+1)])
    toPipe ((x,y), 'L') = ((x,y), [(x-1,y), (x,y+1)])
    toPipe ((x,y), 'J') = ((x,y), [(x-1,y), (x,y-1)])
    toPipe ((x,y), '7') = ((x,y), [(x+1, y), (x, y-1)])
    toPipe ((x,y), 'F') = ((x,y), [(x,y+1), (x+1, y)])
    toPipe ((x,y), 'S') = ((x,y), [(x+1,y), (x-1,y), (x,y-1), (x,y+1)])
    toPipe ((x,y), '.') = ((x,y), [])

-- s0 = 114, 35
solve :: Coord -> Pipelines -> (Int, Int) -- [[Coord]]
solve s0 pipelines = (part1, part2)
  where
    loop  = dfs s0 s0 -- (last $ pipelines Map.! s0) -- bfs (Set.singleton s0) [s0]
    part1 = length loop `div` 2 
    part2 = (abs (shoelace loop) - length loop + 3) `div` 2 - 1

    shoelace [_] = 0
    shoelace ((x0, y0) : (x1, y1) : xs) = (x1 - x0) * (y0 + y1) + shoelace ((x1,y1):xs)

    dfs s s'
      | nextS == s0 = [s0]
      | otherwise   = s : dfs nextS s -- (last $ pipelines Map.! s)
      where 
        nextS = head [s'' | s'' <- pipelines Map.! s, s'' /= s']

main :: IO ()
main = uncurry solve . parser . lines <$> readFile "inputs/2023/input10.txt"
         >>= print
