module Main ( main ) where 

import Data.List ( splitAt )
import Data.Maybe ( fromJust )
import Data.Foldable ( for_ )
import Data.SBV
import Data.Bifunctor ( bimap )
import Rec 

parse :: String -> ([Int], [Int])
parse = splitAt 3 . map read . words . filter (not . (`elem` ",@")) 

intersect p1 p2
  | den == 0 || lo < 0 || hi < 0 = Nothing 
  | otherwise = Just (x1 + lo * vx1, y1 + lo * vy1)
  where
    ([x1, y1, _], [vx1, vy1, _]) = bimap (map fromIntegral) (map fromIntegral) p1
    ([x2, y2, _], [vx2, vy2, _]) = bimap (map fromIntegral) (map fromIntegral) p2
    den                          = vx1 * vy2 - vy1 * vx2
    lo                           = ((x2 - x1) * vy2 - (y2 - y1) * vx2) / den
    hi                           = ((x2 - x1) * vy1 - (y2 - y1) * vx1) / den

getArea lo hi = cata alg . fromList . combs
  where
    combs xs = [(a, b) | a <- xs, b <- xs, a < b]

    alg NilF         = 0
    alg (ConsF x xs) = if inArea (uncurry intersect x) then 1 + xs else 0 + xs

    inArea (Just (x, y)) = lo <= x && x <= hi && lo <= y && y <= hi
    inArea Nothing = False

satSolve xs = sat $ do
    [x, y, z, vx, vy, vz] <- sReals ["x", "y", "z", "vx", "vy", "vz"]

    for_ xs $ \(i, ([hx, hy, hz], [hvx, hvy, hvz])) -> do
        t <- sReal ("t" ++ show i)
        constrain $ t .> 0
        constrain $ fromIntegral hx + t * fromIntegral hvx .== x + t * vx 
        constrain $ fromIntegral hy + t * fromIntegral hvy .== y + t * vy 
        constrain $ fromIntegral hz + t * fromIntegral hvz .== z + t * vz

part2 :: [([Int], [Int])] -> IO Int
part2 xs = do
    res  <- satSolve $ zip [1..] xs
    let x = fromJust $ "x" `getModelValue` res
    let y = fromJust $ "y" `getModelValue` res
    let z = fromJust $ "z" `getModelValue` res
    let s = (x + y + z) :: AlgReal
    return $ read . takeWhile (/= '.') . show $ s

main :: IO () 
main = do input <- map parse . lines <$> readFile "inputs/2023/input24.txt"
          print $ getArea 200000000000000 400000000000000 input
          p2 <- part2 input
          print p2
