module Y2022.Day12 ( solution ) where

import Data.Array (Array, (!), array, bounds, assocs)
import qualified Data.Set as S
import Data.List ( find )
import Data.Maybe ( mapMaybe )

parse :: String -> (Array (Int, Int) Int, ((Int, Int), (Int, Int)))
parse = go (0, 0) ((0,0), (0,0)) []
  where
      go _ endpoints xs []             = let xy = fst (head xs) in (array ((0,0), xy) xs, endpoints)
      go (x, y) (_, end) xs ('S':cs)   = go (x, y+1) ((x,y), end) (((x,y), 0):xs) cs
      go (x, y) (st, _) xs ('E':cs)    = go (x, y+1) (st, (x,y)) (((x,y), 25):xs) cs
      go (x, _) endpoints xs ('\n':cs) = go (x+1, 0) endpoints xs cs
      go (x, y) endpoints xs (c:cs)    = go (x, y+1) endpoints (((x,y), fromEnum c - delta):xs) cs
      delta = fromEnum 'a'

bfs :: Array (Int, Int) Int -> (Int, Int) -> [((Int, Int), Int)]
bfs hmap s = go S.empty [(s, 0)] []
  where
    go _    [] [] = []
    go hist [] ys = go hist (reverse ys) []
    go hist ((coord,c):xs) ys
      | coord `S.member` hist = go hist xs ys
      | otherwise             = (coord, c) : go (S.insert coord hist) xs (next <> ys)
      where 
          next = nextStates coord
          nextStates (x, y) = [ ((x', y'), c+1) | x' <- [x-1 .. x+1]
                                                , y' <- [y-1 .. y+1]
                                                , x' == x || y' == y
                                                , (x', y') `S.notMember` hist
                                                , (x', y') `inBound` bounds hmap
                                                , hmap ! (x', y') <= hmap ! (x, y) + 1
                              ]
          inBound (x, y) ((lx, ly), (hx, hy)) = x >= lx && x <= hx && y >= ly && y <= hy

solution :: IO ()
solution = do (area, (st, end)) <- parse <$> readFile "inputs/2022/input12.txt"
              let search       = find ((==end) . fst) . bfs area
                  alternatives = map fst $ filter ((==0) . snd) $ assocs area
              print $ search st
              print $ minimum $ map snd $ mapMaybe search alternatives
