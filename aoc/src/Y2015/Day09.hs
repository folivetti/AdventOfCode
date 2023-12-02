{-# language OverloadedStrings #-}
module Main where

import Utils ( runParser )
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8 ( decimal, string, Parser )
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Map as M

type Graph = M.Map B.ByteString (M.Map B.ByteString Int)

createEdge :: k1 -> k2 -> a -> M.Map k1 (M.Map k2 a)
createEdge from to = M.singleton from . M.singleton to

parseEdges :: Parser Graph
parseEdges = do
  from <- P.takeWhile (/=' ')
  _    <- string " to "
  to   <- P.takeWhile (/=' ')
  _    <- string " = "
  x    <- decimal
  pure $ M.unions [createEdge from to x, createEdge to from x]


search :: Graph -> [(Int, [B.ByteString])]
search graph = go $ [(0, [k]) | k <- M.keys graph]
  where
      go sols
        | solved sols = sols
        | otherwise = go $ concatMap expand sols

      expand (c, x:xs) = [(c + c', k:x:xs) | (k, c') <- candidates x xs]
      
      candidates x xs = let cands = M.toList $ graph M.! x
                        in  filter ((`notElem` xs) . fst) cands
      solved = all (\(_, xs) -> length xs == n)
      n      = length graph

main :: IO ()
main = do
  content <- B.lines <$> B.readFile "inputs/2015/input09.txt"
  let graph = M.unionsWith M.union $ map (runParser parseEdges) content
      sols  = search graph
  print $ minimum sols
  print $ maximum sols
