module Y2015.Day10 (solution) where

import Data.List ( group )

input :: String
input = "1113222113"

rebuild :: String -> String
rebuild = concatMap f . group
  where
    f x = show (length x) <> [head x]
{-# INLINE rebuild #-}

solution :: IO ()
solution = do
    let sol = iterate rebuild input
    print $ length $ sol !! 40
    print $ length $ sol !! 50
