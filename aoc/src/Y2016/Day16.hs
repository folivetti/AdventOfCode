module Y2016.Day16 ( solution ) where

import Data.List ( find, foldl' )

step :: String -> String
step xs = xs <> ('0' : change xs)
  where
    change      = foldl' (\acc x -> flipBit x : acc ) ""
    flipBit '0' = '1'
    flipBit '1' = '0'
    flipBit  _  = error "wrong bit"

checksum :: String -> String
checksum []  = []
checksum [_] = error "odd length vec"
checksum (x:y:xs)
  | x == y    = '1' : checksum xs
  | otherwise = '0' : checksum xs

myInput :: String
myInput = "11100010111110100"

solve :: Int -> String -> Maybe String
solve n x = expand x >>= (getCheck . take n)
  where
    expand   = find ((>n) . length) . iterate step
    getCheck = find (odd . length) . iterate checksum

solution :: IO ()
solution = do 
    print $ solve 272 myInput
    print $ solve 35651584 myInput
