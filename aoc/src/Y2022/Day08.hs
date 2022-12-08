module Y2022.Day08 ( solution ) where

import qualified Numeric.LinearAlgebra.Data as LA
import Numeric.LinearAlgebra.Data ( Extractor(..), idxs, (??) )
import Data.Char ( digitToInt )

parse :: [String] -> LA.Matrix Double
parse = LA.fromLists . map (map (fromIntegral . digitToInt))

allLess :: Ord a => [a] -> Bool
allLess []     = False
allLess (x:xs) = all (<x) xs

allCoords :: LA.Matrix Double -> [(Int, Int)]
allCoords mtx = (,) <$> [0 .. m-1] <*> [0 .. n-1]
  where (m, n) = LA.size mtx

countVisibles :: LA.Matrix Double -> Int
countVisibles mtx = howMany mtx
  where
    howMany   = length . filter isVisible . allCoords 
    isVisible = any allLess . genCoords mtx

viewScore :: LA.Matrix Double -> Int
viewScore mtx = getMax mtx
  where
    getMax     = maximum . map visibility . allCoords
    visibility = product . map getView . genCoords mtx

    getView []     = 0
    getView (x:xs) = length $ getBlock x xs

    getBlock _ [] = []
    getBlock x (y:ys)
      | y < x     = y : getBlock x ys
      | otherwise = [y]

genCoords :: LA.Matrix Double -> (Int, Int) -> [[Double]]
genCoords mtx (x, y) = map slice [ (Pos (idxs[x]), Range y (-1) 0)
                                 , (Pos (idxs[x]), Range y 1 (n-1))
                                 , (Range x (-1) 0, Pos (idxs[y]))
                                 , (Range x 1 (m-1), Pos (idxs[y]))
                                 ]
  where
    (m, n) = LA.size mtx
    slice  = concat . LA.toLists . (mtx ??)

solution :: IO ()
solution = do mtx <- parse . lines <$> readFile "inputs/2022/input08.txt"
              print $ countVisibles mtx
              print $ viewScore mtx
