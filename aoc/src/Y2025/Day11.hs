module Main where 

import Control.Arrow 
import Data.List ( span, (\\) )
import Utils
import qualified Data.Map.Strict as M 
import Data.Map.Strict ( Map )
import Control.Monad.State 
import Control.Monad ( forM )

parse :: [String] -> Map String [String]
parse = M.fromList . map ((\(a:as) -> (init a,as)) . words)

dfs :: Map String [String] -> String -> String -> Int
dfs graph node end  = go node `evalState` M.empty
  where 
    go :: String -> State (Map String Int) Int 
    go cur 
      | cur == end   = pure 1
      | cur == "out" = pure 0
      | otherwise    = do
          visited <- get
          case visited M.!? cur of
            Just n  -> do modify (M.insert cur n)
                          pure n
            Nothing -> do 
              tot <- forM (graph M.! cur) (\n -> do
                            res <- go n
                            pure res)
              modify (M.insert cur (sum tot))
              pure (sum tot)

solve1 :: Map String [String] -> Int
solve1 graph = dfs graph "you" "out"

solve2 graph = let p1 = dfs graph "svr" "fft"
                   p2 = dfs graph "fft" "out"
                   p3 = dfs graph "svr" "dac"
                   p4 = dfs graph "dac" "out"
                   p5 = dfs graph "fft" "dac"
                   p6 = dfs graph "dac" "fft"
                in p1*p5*p4 + p2*p6*p3

main :: IO ()
main = (solve1 &&& solve2) . parse . lines <$> readFile "inputs/2025/input11.txt"
         >>= print
