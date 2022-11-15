module Y2016.Day02 ( solution ) where

import Data.Map.Strict ( Map, (!) )
import qualified Data.Map.Strict as M
import Data.Bifunctor ( bimap, first, second )

data Neighbors = N { _left   :: String
                   , _right  :: String
                   , _top    :: String
                   , _bottom :: String
                   } deriving Show

type Graph = Map String Neighbors

part1, part2 :: Graph
part1 = M.fromList [ ("1", N "1" "2" "1" "4")
                   , ("2", N "1" "3" "2" "5")
                   , ("3", N "2" "3" "3" "6")
                   , ("4", N "4" "5" "1" "7")
                   , ("5", N "4" "6" "2" "8")
                   , ("6", N "5" "6" "3" "9")
                   , ("7", N "7" "8" "4" "7")
                   , ("8", N "7" "9" "5" "8")
                   , ("9", N "8" "9" "6" "9")
                   ]
part2 = M.fromList [ ("1", N "1" "1" "1" "3")
                   , ("2", N "2" "3" "2" "6")
                   , ("3", N "2" "4" "1" "7")
                   , ("4", N "3" "4" "4" "8")
                   , ("5", N "5" "6" "5" "5")
                   , ("6", N "5" "7" "2" "A")
                   , ("7", N "6" "8" "3" "B")
                   , ("8", N "7" "9" "4" "C")
                   , ("9", N "8" "9" "9" "9")
                   , ("A", N "A" "B" "6" "A")
                   , ("B", N "A" "C" "7" "D")
                   , ("C", N "B" "C" "8" "C")
                   , ("D", N "D" "D" "B" "D")
                   ]

up, down, right, left :: Graph -> String -> String
up g    = _top . (g !)
down g  = _bottom . (g !)
left g  = _left . (g !)
right g = _right . (g !)

toMoves :: Graph -> String -> String -> String
toMoves _ ""          = id
toMoves g ('U':moves) = toMoves g moves . up g
toMoves g ('D':moves) = toMoves g moves . down g
toMoves g ('R':moves) = toMoves g moves . right g
toMoves g ('L':moves) = toMoves g moves . left g
toMoves _ _           = error "Unknown move"

applyMoves :: Graph -> [String] -> String -> [String]
applyMoves _ [] _ = []
applyMoves g (m:ms) c = let c' = toMoves g m c in c' : applyMoves g ms c'

solution :: IO ()
solution = do
  content <- lines <$> readFile "inputs/2016/input02.txt"
  let solve g = concat $ applyMoves g content "5"
  putStrLn $ solve part1
  putStrLn $ solve part2
