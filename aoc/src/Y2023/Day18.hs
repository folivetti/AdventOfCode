module Main ( main ) where 

import Utils ( toCoord )
import Rec
import Data.Char ( isDigit )
import Control.Arrow ( (&&&) )

type Coord = (Int, Int) 

east, west, south, north :: Coord
east  = (0, 1)
west  = (0, -1)
south = (1, 0)
north = (-1, 0)

(x, y) .+. (a, b) = (x+a, y+b)
c *. (x, y)       = (c * x, c * y)

parser (d:n:color:_) = case d of
                         "R" -> (read n *. east, parseColor color)
                         "L" -> (read n *. west, parseColor color)
                         "D" -> (read n *. south, parseColor color)
                         "U" -> (read n *. north, parseColor color)

parseColor xs = stp *. d 
  where 
    ys  = filter (`notElem` "()#") xs
    stp = read ("0x" <> (take 5 ys))
    d   = case last ys of 
            '0' -> east 
            '1' -> south 
            '2' -> west 
            '3' -> north

swap (x, y) = (y, x) 

solve ds = (area + len) `div` 2 + 1
  where 
    vertices    = ana coalg ((0, 0), ds)
    (len, area) = histo alg vertices
    s0          = case vertices of
                     Fix (ConsF x _) -> x 
                     _ -> error "null vertices"

    alg NilF            = (0, 0)
    alg (ConsF (x, y) table) =
        case nextElem table of 
          Nothing     -> (abs (fst s0) + abs (snd s0), 0)
          Just (a, b) -> (abs (x-a) + abs (y - b) + l, (a - x) * (b + y) + s)  -- len and shoelace at the same time
      where (l, s) = extract table 

    coalg (_, []) = NilF
    coalg (v, x:xs) = ConsF (v .+. x) (v .+. x, xs) 

main :: IO ()
main = ((solve . map fst) &&& (solve . map snd)) . map (parser . words) . lines <$> readFile "inputs/2023/input18.txt"
         >>= print

