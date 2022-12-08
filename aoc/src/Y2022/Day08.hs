module Y2022.Day08 ( solution ) where

import qualified Numeric.LinearAlgebra.Data as LA
import Numeric.LinearAlgebra.Data ( Extractor(..), idxs, (??) )
import Data.Char ( digitToInt )

parse :: [String] -> LA.Matrix Double
parse = LA.fromLists . map (map (fromIntegral . digitToInt))

allLess :: Ord a => [a] -> Bool
allLess []     = error "wth?"
allLess [_]    = True
allLess (x:xs) = maximum xs < x

countVisibles :: LA.Matrix Double -> Int
countVisibles mtx = foldr addIfVisible 0 coords
  where
    (m, n)              = LA.size mtx
    coords              = (,) <$> [0 .. m-1] <*> [0 .. n-1]
    addIfVisible xy acc = if isVisible xy
                            then acc + 1
                            else acc

    isVisible = any allLess . genCoords mtx

viewScore :: LA.Matrix Double -> Int
viewScore mtx = maximum $ map visibility coords
  where
    (m, n) = LA.size mtx
    coords = (,) <$> [0 .. m-1] <*> [0 .. n-1]
    visibility = product . map getView . genCoords mtx
    getView []     = error "wth"
    getView [_]    = 0
    getView (x:xs) = length $ getBlock x xs
    getBlock x [] = []
    getBlock x (y:ys)
      | y < x = y : getBlock x ys
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
solution = do content <- lines <$> readFile "inputs/2022/input08.txt"
              let mtx = parse content
              print $ countVisibles mtx
              print $ viewScore mtx
