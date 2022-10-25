{-# language OverloadedStrings #-}
module Y2015.Day09 (solution) where

import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Map as M

type Graph = M.Map B.ByteString (M.Map B.ByteString Int)

createEdge from to = M.singleton from . M.singleton to

parseEdges :: Parser Graph
parseEdges = do
  from <- P.takeWhile (/=' ')
  _    <- string " to "
  to   <- P.takeWhile (/=' ')
  _    <- string " = "
  x    <- decimal
  pure $ M.unions [createEdge from to x, createEdge to from x]

runParser :: Parser a -> B.ByteString -> a
runParser parser dat = case parse parser dat of
                  Done _ x -> x
                  Partial p -> case p "" of
                                 Done _ x -> x
                                 _ -> error "no parse"
                  _ -> error "no parse"
{-# INLINE runParser #-}

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

solution :: IO ()
solution = do
  content <- B.lines <$> B.readFile "inputs/2015/input09.txt"
  let graph = M.unionsWith M.union $ map (runParser parseEdges) content
      sols  = search graph
  print $ minimum sols
  print $ maximum sols
