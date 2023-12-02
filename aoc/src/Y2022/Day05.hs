module Main where

import Data.List ( transpose )
import Data.List.Split ( chunksOf, splitOn )
import qualified Data.IntMap as M

getCrates :: [String] -> M.IntMap String
getCrates = M.fromList . zip [1..] . map (dropWhile (==' ')) . transpose . map (map (!! 1) . chunksOf 4) . take 8

getMoves :: [String] -> [(Int, Int, Int)]
getMoves = map (getVals . splitOn " ") . drop 10

getVals :: [String] -> (Int, Int, Int)
getVals xs = (read (xs !! 1), read (xs !! 3), read (xs !! 5))

makeMoveWith :: (String -> String) -> M.IntMap String -> (Int, Int, Int) -> M.IntMap String
makeMoveWith f xs (n, x, y) = M.insert x x' $ M.insert y y' xs
  where
    cratesX = xs M.! x
    cratesY = xs M.! y
    x'      = drop n cratesX
    y'      = f (take n cratesX) <> cratesY

makeMovesWith :: (String -> String) -> M.IntMap String -> [(Int, Int, Int)] -> String
makeMovesWith f crates = M.elems . M.map head . foldl (makeMoveWith f) crates

main :: IO ()
main = do content <- lines <$> readFile "inputs/2022/input05.txt"
          let crates = getCrates content
              moves  = getMoves content
          print $ makeMovesWith reverse crates moves
          print $ makeMovesWith id crates moves
