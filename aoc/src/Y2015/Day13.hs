{-# language OverloadedStrings #-}
module Y2015.Day13 (solution) where

import Utils ( runParser )
import Data.List ( maximumBy )
import Data.Function ( on )
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8 ( decimal, string, Parser )
import qualified Data.Attoparsec.ByteString.Char8 as P
import Control.Applicative ( (<|>) )

import qualified Data.Map.Strict as M

type Graph = M.Map B.ByteString (M.Map B.ByteString Int)

createEdge :: k1 -> k2 -> a -> M.Map k1 (M.Map k2 a)
createEdge from to = M.singleton from . M.singleton to

parseEdges :: Parser Graph
parseEdges = do
  from <- P.takeWhile (/=' ')
  _    <- string " would "
  f    <- parseLose <|> parseWin
  x    <- f <$> decimal
  _    <- string " happiness units by sitting next to "
  to   <- P.takeWhile (/='.')
  pure $ M.unions [createEdge from to x]

parseLose :: Parser (Int -> Int)
parseLose = string "lose " >> pure negate

parseWin :: Parser (Int -> Int)
parseWin = string "gain " >> pure id

search :: Graph -> [(Int, [B.ByteString])]
search graph = go $ [(0, [k], k) | k <- M.keys graph]
  where
      go sols
        | solved sols = map addLast sols
        | otherwise = go $ concatMap expand sols

      expand (c, x:xs, st) = [(c + c' + graph M.! k M.! x, k:x:xs, st) | (k, c') <- candidates x xs]
      
      candidates x xs = let cands = M.toList $ graph M.! x
                        in  filter ((`notElem` xs) . fst) cands
      solved = all (\(_, xs, _) -> length xs == n)
      addLast (c, x:xs, st) = (c + graph M.! x M.! st + graph M.! st M.! x, x:xs)
      n      = length graph

addYourself :: Graph -> Graph
addYourself graph = M.union youGraph graph'
  where
    youGraph = M.singleton "You" $ M.fromList [(k,0) | k <- M.keys graph]
    graph'   = M.map (M.insert "You" 0) graph

solution :: IO ()
solution = do
  content <- B.lines <$> B.readFile "inputs/2015/input13.txt"
  let graph = M.unionsWith M.union $ map (runParser parseEdges) content
      sols  = search graph
      sols' = search $ addYourself graph
  print $ maximumBy (compare `on` fst) sols
  print $ maximumBy (compare `on` fst) sols'
