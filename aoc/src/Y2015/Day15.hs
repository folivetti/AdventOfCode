module Main where

import Data.List ( transpose )

sprinkles, peanutButter, frosting, sugar :: [Int]
sprinkles    = [5, -1, 0, 0, 5]
peanutButter = [-1, 3, 0, 0, 1]
frosting     = [0, -1, 4, 0, 6]
sugar        = [-1, 0, 0, 2, 8]

ingredients :: [[Int]]
ingredients = transpose [sprinkles, peanutButter, frosting, sugar]

properties :: [[Int]]
properties = init ingredients

cals :: [Int]
cals = last ingredients

score :: [Int] -> (Int, Int)
score qtd = (sc, totCal)
  where 
    p      = max 0 . sum . zipWith (*) qtd
    sc     = product $ map p properties
    totCal = sum $ zipWith (*) qtd cals


allCombs :: [[Int]]
allCombs = [[w, x, y, z] | w <- [0 .. 100]
                         , x <- [0 .. 100]
                         , y <- [0 .. 100]
                         , z <- [0 .. 100]
                         , w + x + y + z == 100
           ]
main :: IO ()
main = do
  let scs = map score allCombs
  print $ fst $ maximum scs
  print $ fst $ maximum $ filter ((==500) . snd) scs
