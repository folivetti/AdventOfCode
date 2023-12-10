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

solve :: Coord -> Pipelines -> (Int, Int) 
solve s0 pipelines = (part1 `div` 2, part2) 
  where 
    loop = ana coalg (s0, s0) 

    part1 = cata alg loop
    part2 = (histo alg2 loop - part1) `div` 2 - 1

    alg NilF         = 1
    alg (ConsF _ xs) = xs + 1

    alg2 NilF = 3
    alg2 (ConsF x table) = 
        case nextElem table of 
          Nothing -> extract table + (fst s0 - fst x) * (snd s0 + snd x) 
          Just y  -> extract table + (fst y - fst x) * (snd y + snd x)

    coalg :: (Coord, Coord) -> ListF Coord (Coord, Coord)
    coalg (s, s')
      | nextS == s0 = NilF
      | otherwise   = ConsF s (nextS, s) 
      where 
        nextS = head [s'' | s'' <- pipelines Map.! s, s'' /= s']

main :: IO ()
main = uncurry solve . parser . lines <$> readFile "inputs/2023/input10.txt"
         >>= print
