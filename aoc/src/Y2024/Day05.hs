{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import Utils
import qualified Data.IntMap.Strict as IM
import Data.List.Split ( splitOn )

splitData :: [String] -> (IM.IntMap [Int], [[Int]])
splitData xss = (IM.fromListWith (<>) . map parsePrecedence $ takeWhile (/= "") xss, map parseSeq . tail $ dropWhile (/= "") xss)
  where
    parsePrecedence [a,b,_,c,d] = (read [c,d], [read [a,b]])
    parseSeq = map read . splitOn ","

solve x = (solve1 x, solve2 x)

solve1 (precedence, updates) = sum $ map getMiddle $ filter isOk updates
  where
    getMiddle xs = let n = length xs in xs !! (n `div` 2)

    isOk xs = go xs
      where
        go [] = True
        go (x:xs) = if x `IM.member` precedence && any (`elem` xs) (precedence IM.! x)
                       then False
                       else go xs

solve2 (precedence, updates) = sum $ map (getMiddle . fix 0 . toMap) $ filter isNotOk updates
  where
    getMiddle xs = let n = IM.size xs in xs IM.! (n `div` 2)
    toMap = IM.fromList . zip [0..]

    fix ix m | ix == IM.size m = m
    fix ix m = let x  = m IM.! ix
                   xs = precedence IM.! x
                   kvs = IM.toAscList $ IM.filterWithKey (\k y -> k > ix && y `elem` xs) m
               in if null kvs
                     then fix (ix+1) m
                     else let (k, v) = head kvs
                          in fix ix $ IM.insert ix v $ IM.insert k x m


    isNotOk xs = go xs
      where
        go [] = False
        go (x:xs) = if x `IM.member` precedence && any (`elem` xs) (precedence IM.! x)
                       then True
                       else go xs


main :: IO ()
main = solve . splitData . lines <$> readFile "inputs/2024/input05.txt"
        >>= print
