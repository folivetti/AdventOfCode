{-# language TransformListComp #-}
module Main where

import Data.Bits ( popCount )
import Data.Set ( Set )
import qualified Data.Set as S

data Path = Path { _steps :: Int
                 , _coord :: (Int, Int)
                 } deriving (Show, Eq)

favNum :: Int
favNum = 1352
    
isEmptySpace :: (Int, Int) -> Bool
isEmptySpace (x, y) = x >= 0 && y >= 0 && even (popCount (x*x + 3*x + 2*x*y + y + y*y + favNum))

s0 :: Path
s0 = Path 0 (1, 1)

nextStates :: Path -> [Path]
nextStates (Path s c) = [ Path (s+1) c' | c' <- neighbors c, isEmptySpace c']

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

bfs :: Path -> [Path]
bfs s = go S.empty [s] []
  where
    go _    [] [] = []
    go hist [] ys = go hist (reverse ys) []
    go hist (x:xs) ys
      | coord `S.member` hist = go hist xs ys
      | otherwise             = x : go (S.insert coord hist) xs (nextStates x <> ys)
      where coord = _coord x

main :: IO ()
main = do let paths = bfs s0
          mapM_ print [ cost | Path cost (31, 39) <- paths ]
          print $ length [ () | Path costs _ <- paths, 
                                then takeWhile by costs <= 50 ]
