module Main where

import Control.Arrow 
import Utils
import qualified Data.Set as Set 
import Data.Set (Set)
import Debug.Trace

type Action = [Int]

splitOn :: Char -> String -> [String]
splitOn c s = case dropWhile (== c) s of
                  "" -> []
                  s' -> w : splitOn c s''
                        where (w, s'') = break (== c) s'


parse :: String -> ([Bool], [Action], [Int])
parse input = (initialState, actions, jolts)
  where
    xs = words input
    states = init $ tail $ head xs
    joltsSet = splitOn ',' $ init $ tail $ last xs 
    actionsList = init $ tail $ xs

    initialState = map (== '#') states
    actions = map parseAction actionsList
    jolts = map read joltsSet

    parseAction as = map read $ splitOn ',' $ init $ tail as 

applyAction :: [Bool] -> Action -> [Bool]
applyAction state action = [ if i `elem` action then not (state !! i) else state !! i | i <- [0..length state - 1] ]

applyAction2 :: [Int] -> Action -> [Int]
applyAction2 jolts action = [ jolts !! i + inc | i <- [0 .. length jolts - 1], let inc = if i `elem` action then 1 else 0 ]

bfs :: Set [Bool] -> [[Bool]] -> [Bool] -> [Action] -> Int -> Int
bfs visited candidates goal actions cost
  | any (==goal) candidates = cost
  | otherwise = bfs visited' newCandidates goal actions (cost + 1)
    where
      (currentState:rest) = candidates
      visited' = Set.union visited (Set.fromList candidates)
      newCandidates = [ newState
                      | action <- actions
                      , cand <- candidates 
                      , let newState = applyAction cand action
                      , not (Set.member newState visited') ]

bfs2 :: Set [Int] -> [[Int]] -> [Int] -> [Action] -> Int -> Int
bfs2 visited candidates goal actions cost
  | any (==goal) candidates = cost
  | otherwise = bfs2 visited' newCandidates goal actions (cost + 1)
    where
      (currentState:rest) = candidates
      visited' = Set.union visited (Set.fromList candidates)
      newCandidates = [ newState
                      | action <- actions
                      , cand <- candidates 
                      , let newState = applyAction2 cand action
                      , not (Set.member newState visited') ]

solve1 = foldr applyBFS 0
  where
    applyBFS (goal, actions, _) acc = acc + bfs (Set.singleton [False | _ <- goal]) [[False | _ <- goal]] goal actions 0
solve2 = foldr applyBFS2 0
  where
    applyBFS2 ( _, actions, jolts) acc = acc + bfs2 (Set.singleton [0 | _ <- jolts]) [[0 | _ <- jolts]] jolts actions 0

main :: IO ()
main = (solve1 &&& solve2) . map parse . lines <$> readFile "inputs/2025/input10.txt"
         >>= print
