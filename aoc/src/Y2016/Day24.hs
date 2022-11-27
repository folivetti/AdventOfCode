{-# LANGUAGE TupleSections #-}
module Y2016.Day24 ( solution ) where

import Data.Array (Array, (!), array)
import Data.Char (isDigit)
import Data.List (permutations, sort, find)
import Data.Maybe (fromJust)
import qualified Data.Map as M (Map, (!), fromList)
import qualified Data.Set as S

getCoords :: [String] -> [(Int,Int)]
getCoords xs = map snd $ sort [ (r, (x,y)) | (x, row) <- enumerate xs
                                           , (y, r)   <- enumerate row
                                           , isDigit r
                              ]

enumerate :: [b] -> [(Int, b)]
enumerate = zip [0..]

parse :: [String] -> Array (Int, Int) Bool
parse xs = array ((0, 0), (m, n)) [((x,y), c /= '#') | (x, row) <- enumerate xs
                                                     , (y, c)   <- enumerate row
                                  ]
  where
    m = length xs - 1
    n = length (head xs) - 1

walk :: M.Map ((Int, Int), (Int, Int)) Int -> [(Int, Int)] -> Int
walk dist order = sum [dist M.! pair | pair <- zip order $ tail order]

calculateDistances :: Array (Int, Int) Bool -> [(Int, Int)] -> M.Map ((Int, Int), (Int, Int)) Int
calculateDistances ducts bots = M.fromList dists
  where
    dists = [((xy, xy'), getDist xy xy') | xy <- bots, xy' <- bots, xy /= xy']
    getDist xy xy' = snd $ fromJust $ find ((==xy') . fst) $ bfs ducts (xy, 0)

nextStates :: Array (Int, Int) Bool -> (Int, Int) -> [(Int, Int)]
nextStates ducts xy = [ xy' | xy' <- neighbors xy, ducts ! xy']

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

bfs :: Array (Int, Int) Bool -> ((Int, Int), Int) -> [((Int, Int), Int)]
bfs ducts s = go S.empty [s] []
  where
    go _    [] [] = []
    go hist [] ys = go hist (reverse ys) []
    go hist ((coord, c):xs) ys
      | coord `S.member` hist = go hist xs ys
      | otherwise             = (coord, c) : go (S.insert coord hist) xs (next <> ys)
      where next = map (,c+1) $ nextStates ducts coord

solution :: IO ()
solution = do
    content <- lines <$> readFile "inputs/2016/input24.txt"
    let (start:others) = getCoords content
        candidates     = map (start :) $ permutations others
        ducts          = parse content
        dist           = calculateDistances ducts (start:others)
    print $ minimum $ map (walk dist) candidates
    print $ minimum $ map (walk dist . (<> [start])) candidates
