{-# language OverloadedStrings #-}
{-# language TupleSections #-}
module Y2016.Day22 ( solution ) where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List ( find, sortOn )
import Utils ( runParser )

data Node = Node {  _size  :: Int
                  , _used  :: Int
                  , _avail :: Int
                 } deriving (Show, Eq, Ord)

type Nodes = M.Map (Int, Int) Node

data Place = G | V | E | W deriving (Show, Eq, Ord)

type Stats = (M.Map (Int, Int) Place, Int, (Int, Int))

parser :: Parser Nodes
parser = do string "/dev/grid/node-x"
            x <- decimal
            string "-y"
            y <- decimal
            many' space
            s <- decimal
            char 'T' >> many' space
            u <- decimal
            char 'T' >> many' space
            a <- decimal
            char 'T' >> many' space
            decimal
            char '%'
            pure $ M.singleton (x,y) $ Node s u a

viablePairs :: [((Int, Int), Node)] -> Int
viablePairs nodes = length [ (xy1, xy2) | (xy1, n1) <- nodes
                                        , _used n1 /= 0
                                        , (xy2, n2) <- nodes
                                        , xy1 /= xy2
                                        , _used n1 <= _avail n2
                           ]

nextStates :: Stats -> [Stats]
nextStates (m, c, xy) = [ (swap xy xy', c+1, xy') | xy' <- neighbors xy 
                                                  , m M.! xy' /= W
                    ]
  where
    neighbors (x, y) = filter isValid [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
    isValid (x, y)   = x >= 0 && x <= 31 && y >= 0 && y <= 29
    swap ab cd       = let n2  = m M.! cd
                        in M.insert ab n2 $ M.insert cd E m


bfs :: Stats -> [Stats]
bfs s = go S.empty [s] []
  where
    go _    [] [] = []
    go hist [] ys = go hist (reverse ys) []
    go hist (x:xs) ys
      | xh `S.member` hist = go hist xs ys
      | otherwise          = x : go (S.insert xh hist) xs (nextStates x <> ys)
      where xh = (\(b,_,_) -> b) x

astar :: Stats -> [Stats]
astar s = go S.empty [s]
  where
    go _    [] = []
    go hist (x:xs)
      | xh `S.member` hist = go hist xs
      | otherwise          = x : go (S.insert xh hist) xs'
      where 
          xh             = (\(b,_,_) -> b) x
          xs'            = sortOn heur (xs <> nextStates x)
          heur (b, c, _) = (,c) $ fst $ head $ M.toList $ M.filter (==G) b

solution :: IO ()
solution = do nodes <- M.unions . map (runParser parser) . B.lines <$> B.readFile "inputs/2016/input22.txt"
              let viables = viablePairs $ M.toList nodes
                  board   = M.insert (31,0) G 
                          $ M.fromList $ map (\(c, v) -> (c, if _used v <= 86 then V else W)) 
                          $ M.toList nodes
                  sols = astar (board, 0, (26,22))
                  isGoal (b, _, _) = b M.! (0,0) == G
                  getCost (_, c, _) = c
              print viables
              print $ getCost <$> find isGoal sols
